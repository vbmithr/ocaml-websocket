(*
 * Copyright (c) 2012 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt
open Cohttp
open Cohttp_lwt_unix

module CK = Cryptokit

let base64_encode str =
  let tr = CK.Base64.encode_compact_pad () in
  CK.transform_string tr str

let sha1sum str =
  let hash = CK.Hash.sha1 () in
  CK.hash_string hash str

external ($) : ('a -> 'b) -> 'a -> 'b = "%apply"
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

module Opt = struct
  let map f = function
    | Some x -> Some(f x)
    | None -> None

  let default d = function
    | Some x -> x
    | None -> d

  let unbox = function
    | Some x -> x
    | None -> raise Not_found
end

exception Not_implemented

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

type opcode =
  [ `Continuation
  | `Text
  | `Binary
  | `Close
  | `Ping
  | `Pong
  | `Ctrl of int
  | `Nonctrl of int
  ]

type frame = { opcode : opcode; final : bool; content : string }

let string_of_opcode = function
  | `Continuation -> "continuation frame"
  | `Text         -> "text frame"
  | `Binary       -> "binary frame"
  | `Close        -> "close frame"
  | `Ping         -> "ping frame"
  | `Pong         -> "pong frame"
  | `Ctrl i       -> Printf.sprintf "control frame code %d" i
  | `Nonctrl i    -> Printf.sprintf "non-control frame code %d" i

let opcode_of_int i = match i land 0xf with
  | 0                     -> `Continuation
  | 1                     -> `Text
  | 2                     -> `Binary
  | 8                     -> `Close
  | 9                     -> `Ping
  | 10                    -> `Pong
  | i when i > 2 && i < 8 -> `Nonctrl i
  | i                     -> `Ctrl i

let int_of_opcode = function
  | `Continuation -> 0
  | `Text         -> 1
  | `Binary       -> 2
  | `Close        -> 8
  | `Ping         -> 9
  | `Pong         -> 10
  | `Ctrl i       -> i
  | `Nonctrl i    -> i

let xor mask msg =
  for i = 0 to String.length msg - 1 do (* masking msg to send *)
    msg.[i] <- Char.chr $
      Char.code mask.[i mod 4] lxor Char.code msg.[i]
  done

let read_int16 ic =
  let buf = String.create 2 in
  Lwt_io.read_into_exactly ic buf 0 2 >>
    (return $ EndianString.BigEndian.get_int16 buf 0)

let read_int64 ic =
  let buf = String.create 8 in
  Lwt_io.read_into_exactly ic buf 0 8 >>
    (return $ EndianString.BigEndian.get_int64 buf 0)

let write_int16 oc v =
  let buf = String.create 2 in
  EndianString.BigEndian.set_int16 buf 0 v;
  Lwt_io.write oc buf

let write_int64 oc v =
  let buf = String.create 8 in
  EndianString.BigEndian.set_int64 buf 0 v;
  Lwt_io.write oc buf

let rec read_frames ic push =
  let hdr = String.create 2 in
  lwt () = Lwt_io.read_into_exactly ic hdr 0 2 in
  let hdr = Bitstring.bitstring_of_string hdr in
  let final, rsv1, rsv2, rsv3, opcode, masked, length =
    bitmatch hdr with
      | { final: 1; rsv1: 1; rsv2: 1; rsv3: 1;
          opcode: 4; masked: 1; length: 7 }
        -> final, rsv1, rsv2, rsv3, opcode, masked, length in
  let opcode = opcode_of_int opcode in
  lwt payload_len = (match length with
    | i when i < 126 -> return $ Int64.of_int i
    | 126            -> read_int16 ic >|= Int64.of_int
    | 127            -> read_int64 ic
    | _              -> raise_lwt (Failure "bug in module Bitstring"))
      >|= Int64.to_int
  in
  let mask = String.create 4 in
  lwt () = if masked then Lwt_io.read_into_exactly ic mask 0 4
    else return () in
  let content = String.create payload_len in
  lwt () = Lwt_io.read_into_exactly ic content 0 payload_len in
  let () = if masked then xor mask content in
  let () = push (Some { opcode; final; content }) in
  read_frames ic push

let rec write_frames ~masked stream oc =
  let send_frame fr =
    let mask = CK.Random.string CK.Random.secure_rng 4 in
    let len = String.length fr.content in
    let extensions = 0 in
    let opcode = int_of_opcode fr.opcode in
    let payload_len = match len with
      | n when n < 126      -> len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127 in
    let bitstring = Bitstring.string_of_bitstring $
      BITSTRING {fr.final: 1; extensions: 3;
                 opcode: 4; masked : 1; payload_len: 7} in
    lwt () = Lwt_io.write oc bitstring in
    lwt () =
      (match len with
        | n when n < 126        -> return ()
        | n when n < (1 lsl 16) -> write_int16 oc n
        | n                     -> Int64.of_int n |> write_int64 oc)
    in
    lwt () = if masked then Lwt_io.write_from_exactly oc mask 0 4
        >|= fun () -> xor mask fr.content else return () in
    lwt () = Lwt_io.write_from_exactly oc fr.content 0 len in
    Lwt_io.flush oc in
  Lwt_stream.next stream >>= send_frame >> write_frames ~masked stream oc

let sockaddr_of_dns node service =
  let open Lwt_unix in
  (match_lwt getaddrinfo node service
      [AI_FAMILY(PF_INET); AI_SOCKTYPE(SOCK_STREAM)] with
        | h::t -> return h
        | []   -> raise_lwt Not_found)
      >|= fun ai -> ai.ai_addr

let setup_socket fd = Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true

let open_connection uri =
  (* Initialisation *)
  lwt myhostname = Lwt_unix.gethostname () in
  let host       = Opt.unbox (Uri.host uri) in
  let port       = Opt.default 80 (Uri.port uri) in

  let stream_in, push_in   = Lwt_stream.create ()
  and stream_out, push_out = Lwt_stream.create () in

  let connect () =
    let nonce = base64_encode $ CK.Random.string CK.Random.secure_rng 16 in
    let headers =
      Header.of_list
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = Request.make ~headers uri in
    lwt sockaddr = sockaddr_of_dns host (string_of_int port) in
    lwt ic, oc =
      Lwt_io_ext.open_connection ~setup_socket sockaddr in
    try_lwt
      lwt () = Request.write (fun _ _ -> return ()) req oc in
      lwt response = Response.read ic >>= function
        | Some r -> return r
        | None -> raise_lwt Not_found in
      let headers = Response.headers response in
      (assert_lwt Response.version response = `HTTP_1_1) >>
      (assert_lwt Response.status response = `Switching_protocols) >>
      (assert_lwt Opt.map (fun str -> String.lowercase str)
       $ Header.get headers "upgrade" = Some "websocket") >>
      (assert_lwt Opt.map (fun str -> String.lowercase str)
       $ Header.get headers "connection" = Some "upgrade") >>
      (assert_lwt Header.get headers "sec-websocket-accept" =
         Some (nonce ^ websocket_uuid |> sha1sum |> base64_encode)) >>
      Lwt_log.notice_f "Connected to %s\n%!" (Uri.to_string uri) >>
      return (ic, oc)
    with exn ->
      Lwt_io.close ic <&> Lwt_io.close oc >> raise_lwt exn

  in
  lwt ic, oc = connect () in
  try_lwt
    ignore_result
      (read_frames ic push_in <&> write_frames ~masked:true stream_out oc);
    return (stream_in, push_out)
  with exn -> Lwt_io.close ic <&> Lwt_io.close oc >> raise_lwt exn

let with_connection uri f =
  lwt stream_in, push_out = open_connection uri in
  f (stream_in, push_out)

let establish_server ?buffer_size ?backlog sockaddr f =
  let stream_in, push_in   = Lwt_stream.create ()
  and stream_out, push_out = Lwt_stream.create () in

  let server_fun (ic,oc) =
    lwt request = Request.read ic >>=
      function Some r -> return r | None -> raise_lwt Not_found in
    let meth    = Request.meth request
    and version = Request.version request
    and uri     = Request.uri request
    and headers = Request.headers request in
    let key = Opt.unbox $ Header.get headers "sec-websocket-key" in
    lwt () =
      (assert_lwt version = `HTTP_1_1) >>
      (assert_lwt meth = `GET) >>
      (assert_lwt Opt.map (fun str -> String.lowercase str)
                    $ Header.get headers "upgrade" = Some "websocket") >>
      (assert_lwt Opt.map (fun str -> String.lowercase str)
                    $ Header.get headers "connection" = Some "upgrade")
    in
    let hash = key ^ websocket_uuid |> sha1sum |> base64_encode in
    let response_headers = Header.of_list
      ["Upgrade", "websocket";
       "Connection", "Upgrade";
       "Sec-WebSocket-Accept", hash] in
    let response = Response.make
      ~status:`Switching_protocols
      ~encoding:Transfer.Unknown
      ~headers:response_headers () in
    lwt () = Response.write (fun _ _ -> return ()) response oc
    in
    join [read_frames ic push_in;
          write_frames ~masked:false stream_out oc;
          f uri (stream_in, push_out)]
  in
  Lwt_io_ext.establish_server
    ~setup_server_socket:setup_socket
    ~setup_clients_sockets:setup_socket
    ?buffer_size ?backlog sockaddr
    (fun (ic,oc) -> ignore_result $ server_fun (ic,oc))

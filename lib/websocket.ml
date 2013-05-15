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

module Frame = struct
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

  type t = { opcode    : opcode;
             extension : int;
             final     : bool;
             content   : string }

  let opcode f    = f.opcode
  let extension f = f.extension
  let final f     = f.final
  let content f   = f.content

  let of_string ?(opcode=`Text) ?(extension=0) ?(final=true) content =
    { opcode; extension; final; content }
end

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

let is_bit_set idx v =
  (v lsr idx) land 1 = 1

let set_bit v idx b =
  if b then v lor (1 lsl idx) else v land (lnot (1 lsl idx))

let int_value shift len v =
  (v lsr shift) land ((1 lsl len) - 1)

let rec read_frames ic push =
  let hdr = String.create 2 in
  lwt () = Lwt_io.read_into_exactly ic hdr 0 2 in
  let hdr_part1 = EndianString.BigEndian.get_int8 hdr 0 in
  let hdr_part2 = EndianString.BigEndian.get_int8 hdr 1 in
  let final = is_bit_set 7 hdr_part1 in
  let extension = int_value 4 3 hdr_part1 in
  let opcode = int_value 0 4 hdr_part1 in
  let masked = is_bit_set 7 hdr_part2 in
  let length = int_value 0 7 hdr_part2 in
  let opcode = opcode_of_int opcode in
  lwt payload_len = (match length with
    | i when i < 126 -> return $ Int64.of_int i
    | 126            -> read_int16 ic >|= Int64.of_int
    | 127            -> read_int64 ic
    | _              -> raise_lwt (Failure "internal error"))
      >|= Int64.to_int
  in
  let mask = String.create 4 in
  lwt () = if masked then Lwt_io.read_into_exactly ic mask 0 4
    else return () in
  let content = String.create payload_len in
  lwt () = Lwt_io.read_into_exactly ic content 0 payload_len in
  let () = if masked then xor mask content in
  let () = push (Some (Frame.of_string ~opcode ~extension ~final content)) in
  read_frames ic push

(* Good enough, and do not eat entropy *)
let myrng = CK.Random.pseudo_rng (CK.Random.string CK.Random.secure_rng 20)

let rec write_frames ~masked stream oc =
  let send_frame fr =
    let mask = CK.Random.string myrng 4 in
    let len = String.length (Frame.content fr) in
    let opcode = int_of_opcode (Frame.opcode fr) in
    let payload_len = match len with
      | n when n < 126      -> len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127 in
    let hdr = set_bit 0 15 (Frame.final fr) in (* We do not support extensions for now *)
    let hdr = hdr lor (opcode lsl 8) in
    let hdr = set_bit hdr 7 masked in
    let hdr = hdr lor payload_len in (* Payload len is guaranteed to fit in 7 bits *)
    let hdr_string = String.create 2 in
    let () = EndianString.BigEndian.set_int16 hdr_string 0 hdr in
    lwt () = Lwt_io.write_from_exactly oc hdr_string 0 2 in
    lwt () =
      (match len with
        | n when n < 126        -> return ()
        | n when n < (1 lsl 16) -> write_int16 oc n
        | n                     -> Int64.of_int n |> write_int64 oc)
    in
    lwt () = if masked then Lwt_io.write_from_exactly oc mask 0 4
        >|= fun () -> xor mask (Frame.content fr) else return () in
    lwt () = Lwt_io.write_from_exactly oc (Frame.content fr) 0 len in
    Lwt_io.flush oc in
  Lwt_stream.next stream >>= send_frame >> write_frames ~masked stream oc

let setup_socket = Lwt_io_ext.set_tcp_nodelay

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
    lwt sockaddr = Lwt_io_ext.sockaddr_of_dns host (string_of_int port) in
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
  let server_fun (ic,oc) =
    let stream_in, push_in   = Lwt_stream.create ()
    and stream_out, push_out = Lwt_stream.create () in
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
    pick [read_frames ic push_in;
          write_frames ~masked:false stream_out oc;
          f uri (stream_in, push_out)]
  in
  Lwt_io_ext.establish_server
    ~setup_server_socket:setup_socket
    ~setup_clients_sockets:setup_socket
    ?buffer_size ?backlog sockaddr
    (fun (ic,oc) -> ignore_result $ try_lwt server_fun (ic,oc) with _ -> return ())

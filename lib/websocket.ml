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

external ($)  : ('a -> 'b) -> 'a -> 'b = "%apply"

exception Not_implemented

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

type opcode =
  [ `Continuation
  | `Text
  | `Binary
  | `Close
  | `Ping
  | `Pong
  | `Ctrl
  | `Nonctrl
  ]

let string_of_opcode = function
  | `Continuation -> "continuation frame"
  | `Text         -> "text frame"
  | `Binary       -> "binary frame"
  | `Close        -> "close frame"
  | `Ping         -> "ping frame"
  | `Pong         -> "pong frame"
  | `Ctrl         -> "other control frame"
  | `Nonctrl      -> "other non-control frame"

let opcode_of_int i = match i land 0xf with
  | 0                     -> `Continuation
  | 1                     -> `Text
  | 2                     -> `Binary
  | 8                     -> `Close
  | 9                     -> `Ping
  | 10                    -> `Pong
  | i when i > 2 && i < 8 -> `Nonctrl
  | _                     -> `Ctrl

let int_of_opcode = function
  | `Continuation -> 0
  | `Text         -> 1
  | `Binary       -> 2
  | `Close        -> 8
  | `Ping         -> 9
  | `Pong         -> 10
  | _             -> failwith "int_of_opcode: Invalid frame type"

let xor mask msg =
  for i = 0 to String.length msg - 1 do (* masking msg to send *)
    msg.[i] <- Char.chr $
      Char.code mask.[i mod 4] lxor Char.code msg.[i]
  done

let rec read_frames ic push =
  let hdr = String.create 2 in
  lwt () = Lwt_io.read_into_exactly ic hdr 0 2 in
  let hdr = Bitstring.bitstring_of_string hdr in
  let fin, rsv1, rsv2, rsv3, opcode, masked, length =
    bitmatch hdr with
      | { fin: 1; rsv1: 1; rsv2: 1; rsv3: 1;
          opcode: 4; masked: 1; length: 7 }
        -> fin, rsv1, rsv2, rsv3, opcode, masked, length in
  let opcode = opcode_of_int opcode in
  lwt payload_len = match length with
    | i when i < 126 -> return i
    | 126            -> Lwt_io.BE.read_int16 ic
    | 127            -> Lwt_io.BE.read_int64 ic >|= Int64.to_int
    | _              -> failwith "Can never happen."
  in
  let mask = String.create 2 in
  lwt () = if masked then Lwt_io.read_into_exactly ic mask 0 2
    else return () in
  let buf = String.create payload_len in
  match opcode with
    | `Text ->
      Lwt_io.read_into_exactly ic buf 0 payload_len
      >|= (fun () -> if masked then xor mask buf; push (Some buf))
      >|= (fun () -> if fin then push (Some "")) >> read_frames ic push

    | _ ->
      Lwt_io.read_into_exactly ic buf 0 payload_len
      >> Lwt_log.notice_f
        "Not implemented: Opcode %d, message:\n%s\n%!"
        (int_of_opcode opcode) buf
      >> raise_lwt Not_implemented

let write_frames ~masked stream oc =
  let send_msg ~final ~opcode str =
    let mask = CK.Random.string CK.Random.secure_rng 4 in
    let len = String.length str in
    let first_nibble = 8
    and opcode = int_of_opcode `Text
    and masked = true
    and payload_len = match len with
      | n when n < 126      -> len
      | n when n < 1 lsl 16 -> 126
      | _                   -> 127 in
    let bitstring = Bitstring.string_of_bitstring $
      BITSTRING {first_nibble: 4; opcode: 4; masked : 1; payload_len: 7} in
    lwt () = Lwt_io.write oc bitstring in
    lwt () =
      (match len with
        | n when n < 126        -> return ()
        | n when n < (1 lsl 16) -> Lwt_io.BE.write_int16 oc n
        | n                     -> Lwt_io.BE.write_int64 oc $ Int64.of_int n)
    in
    lwt () = if masked then Lwt_io.write_from_exactly oc mask 0 4
        >|= fun () -> xor mask str else return () in
    lwt () = Lwt_io.write_from_exactly oc str 0 len in
    Lwt_io.flush oc in

  (* Body of the function *)
  let rec main_loop prev opcode =
    lwt next = Lwt_stream.next stream in
    match (prev, next) with
      | "", ""     -> main_loop "" `Text
      | prev, ""   -> send_msg ~final:true ~opcode prev
        >> main_loop "" `Text
      | "", next   -> main_loop next `Text
      | prev, next -> send_msg ~final:false ~opcode prev
        >> main_loop next `Continuation
  in
  main_loop "" `Text

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
    let nonce = Base64.encode $ CK.Random.string CK.Random.secure_rng 16 in
    let headers =
      Header.of_list
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = Request.make ~headers uri in
    lwt sockaddr = sockaddr_of_dns host (string_of_int port) in
    lwt ic, oc =
      Lwt_io.open_connection ~setup_socket sockaddr in
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
          Some (Base64.encode (CK.hash_string (CK.Hash.sha1 ())
                                 (nonce ^ websocket_uuid)))) >>
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
    let hash = Base64.encode
      (CK.hash_string (CK.Hash.sha1 ()) (key ^ websocket_uuid)) in
    let response_headers = Header.of_list
      ["Upgrade", "websocket";
       "Connection", "Upgrade";
       "Sec-WebSocket-Accept", hash] in
    let response = Response.make ~status:`Switching_protocols
      ~headers:response_headers () in
    lwt () = Response.write (fun _ _ -> return ()) response oc
    in
    join [read_frames ic push_in;
          write_frames ~masked:false stream_out oc;
          f uri (stream_in, push_out)]
  in
  Lwt_io.establish_server ~setup_socket ?buffer_size ?backlog sockaddr
    (fun (ic,oc) -> ignore_result $ server_fun (ic,oc))

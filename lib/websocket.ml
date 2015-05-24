(*
 * Copyright (c) 2012-2015 Vincent Bernardoff <vb@luminar.eu.org>
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

let random_string ?(base64=false) size =
  Nocrypto.Rng.generate size |>
  (if base64 then Nocrypto.Base64.encode else fun s -> s) |>
  Cstruct.to_string

let b64_encoded_sha1sum s =
  let open Nocrypto in
  Cstruct.of_string s |>
  Hash.SHA1.digest |>
  Base64.encode |>
  Cstruct.to_string

let websocket_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

module Frame = struct
  module Opcode = struct
    type t =
      | Continuation
      | Text
      | Binary
      | Close
      | Ping
      | Pong
      | Ctrl of int
      | Nonctrl of int [@@deriving show]

    let min = 0x0
    let max = 0xf

    let of_enum = function
      | i when (i < 0 || i > 0xf) -> None
      | 0                         -> Some Continuation
      | 1                         -> Some Text
      | 2                         -> Some Binary
      | 8                         -> Some Close
      | 9                         -> Some Ping
      | 10                        -> Some Pong
      | i when i < 8              -> Some (Nonctrl i)
      | i                         -> Some (Ctrl i)

    let to_enum = function
      | Continuation   -> 0
      | Text           -> 1
      | Binary         -> 2
      | Close          -> 8
      | Ping           -> 9
      | Pong           -> 10
      | Ctrl i         -> i
      | Nonctrl i      -> i

    let is_ctrl opcode = to_enum opcode > 7
  end

  type t = { opcode    : Opcode.t [@default Opcode.Text];
             extension : int [@default 0];
             final     : bool [@default true];
             content   : string [@default ""];
           } [@@deriving show,create]

  let of_bytes ?opcode ?extension ?final content =
    let content = Bytes.unsafe_to_string content in
    create ?opcode ?extension ?final ~content ()

  let close code =
    let content = Bytes.create 2 in
    EndianBytes.BigEndian.set_int16 content 0 code;
    of_bytes ~opcode:Opcode.Close content

  let of_subbytes ?opcode ?extension ?final content pos len =
    let content = Bytes.(sub content pos len |> unsafe_to_string) in
    create ?opcode ?extension ?final ~content ()
end

let xor mask msg =
  for i = 0 to Bytes.length msg - 1 do (* masking msg to send *)
    Bytes.set msg i Char.(code mask.[i mod 4] lxor code (Bytes.get msg i) |> chr)
  done

let is_bit_set idx v =
  (v lsr idx) land 1 = 1

let set_bit v idx b =
  if b then v lor (1 lsl idx) else v land (lnot (1 lsl idx))

let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)

open Lwt.Infix

module C = Cohttp
module CU = Cohttp_lwt_unix

let section = Lwt_log.Section.make "websocket"

let safe_close ic =
  Lwt.catch
    (fun () -> Lwt_io.close ic)
    (fun _ -> Lwt.return_unit)

let read_uint16 ic =
  let buf = Bytes.create 2 in
  Lwt_io.read_into_exactly ic buf 0 2 >|= fun () ->
  EndianBytes.BigEndian.get_uint16 buf 0

let read_int64 ic =
  let buf = Bytes.create 8 in
  Lwt_io.read_into_exactly ic buf 0 8 >|= fun () ->
  EndianBytes.BigEndian.get_int64 buf 0

let write_int16 oc v =
  let buf = Bytes.create 2 in
  EndianBytes.BigEndian.set_int16 buf 0 v;
  Lwt_io.write oc (buf |> Bytes.unsafe_to_string)

let write_int64 oc v =
  let buf = Bytes.create 8 in
  EndianBytes.BigEndian.set_int64 buf 0 v;
  Lwt_io.write oc (buf |> Bytes.unsafe_to_string)

let send_frame ~masked oc fr =
  let open Frame in
  let mask = random_string 4 in
  let content = Bytes.unsafe_of_string fr.content in
  let len = Bytes.length content in
  let opcode = Opcode.to_enum fr.opcode in
  let payload_len = match len with
    | n when n < 126      -> len
    | n when n < 1 lsl 16 -> 126
    | _                   -> 127 in
  let hdr = set_bit 0 15 (fr.final) in (* We do not support extensions for now *)
  let hdr = hdr lor (opcode lsl 8) in
  let hdr = set_bit hdr 7 masked in
  let hdr = hdr lor payload_len in (* Payload len is guaranteed to fit in 7 bits *)
  write_int16 oc hdr >>= fun () ->
  (match len with
   | n when n < 126        -> Lwt.return_unit
   | n when n < (1 lsl 16) -> write_int16 oc n
   | n                     -> Int64.of_int n |> write_int64 oc) >>= fun () ->
  (if masked && len > 0 then begin
      xor mask content;
      Lwt_io.write_from_exactly oc (Bytes.unsafe_of_string mask) 0 4
    end
   else Lwt.return_unit) >>= fun () ->
  Lwt_io.write_from_exactly oc content 0 len >>= fun () ->
  Lwt_io.flush oc

let make_read_frame ~masked (ic,oc) =
  let open Frame in
  let hdr = Bytes.create 2 in
  let mask = Bytes.create 4 in
  let close_with_code code =
    let content = Bytes.create 2 in
    EndianBytes.BigEndian.set_int16 content 0 code;
    send_frame ~masked oc @@ Frame.close code >>= fun () ->
    Lwt.fail Exit in
  fun () ->
    Lwt_io.read_into_exactly ic hdr 0 2 >>= fun () ->
    let hdr_part1 = EndianBytes.BigEndian.get_int8 hdr 0 in
    let hdr_part2 = EndianBytes.BigEndian.get_int8 hdr 1 in
    let final = is_bit_set 7 hdr_part1 in
    let extension = int_value 4 3 hdr_part1 in
    let opcode = int_value 0 4 hdr_part1 in
    let frame_masked = is_bit_set 7 hdr_part2 in
    let length = int_value 0 7 hdr_part2 in
    let opcode = Frame.Opcode.of_enum opcode |> CCOpt.get_exn in
    (match length with
     | i when i < 126 -> Lwt.return @@ Int64.of_int i
     | 126            -> read_uint16 ic >|= Int64.of_int
     | 127            -> read_int64 ic
     | _              -> assert false) >|= Int64.to_int >>= fun payload_len ->
    (if extension <> 0 then close_with_code 1002 else Lwt.return_unit) >>= fun () ->
    (if Opcode.is_ctrl opcode && payload_len > 125 then close_with_code 1002
     else Lwt.return_unit) >>= fun () ->
    (if frame_masked
     then Lwt_io.read_into_exactly ic mask 0 4
     else Lwt.return_unit) >>= fun () ->
    (* Create a buffer that will be passed to the push function *)
    let content = Bytes.create payload_len in
    Lwt_io.read_into_exactly ic content 0 payload_len >>= fun () ->
    let () = if frame_masked then xor (Bytes.unsafe_to_string mask) content in
    let frame = Frame.of_bytes ~opcode ~extension ~final content in
    Lwt_log.debug_f ~section "<- %s" (Frame.show frame) >|= fun () ->
    frame

exception HTTP_Error of string

let set_tcp_nodelay flow =
  let open Conduit_lwt_unix in
  match flow with
  | TCP { fd; _ } -> Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
  | _ -> ()

let with_connection ?(extra_headers = C.Header.init ()) ~ctx client uri =
  let connect () =
    let open Cohttp in
    let nonce = random_string ~base64:true 16 in
    let headers = C.Header.add_list extra_headers
        ["Upgrade"               , "websocket";
         "Connection"            , "Upgrade";
         "Sec-WebSocket-Key"     , nonce;
         "Sec-WebSocket-Version" , "13"] in
    let req = Request.make ~headers uri in
    Conduit_lwt_unix.(connect ~ctx:default_ctx client) >>= fun (flow, ic, oc) ->
    set_tcp_nodelay flow;
    let drain_handshake () =
      Lwt_unix.handle_unix_error
        (fun () -> CU.Request.write
            (fun writer -> Lwt.return_unit) req oc) () >>= fun () ->
      Lwt_unix.handle_unix_error CU.Response.read ic >>= (function
          | `Ok r -> Lwt.return r
          | `Eof -> Lwt.fail End_of_file
          | `Invalid s -> Lwt.fail @@ Failure s)
      >>= fun response ->
      let status = Response.status response in
      let headers = CU.Response.headers response in
      if Code.(is_error @@ code_of_status status)
      then Lwt.fail @@ HTTP_Error Code.(string_of_status status)
      else if not (Response.version response = `HTTP_1_1
                   && status = `Switching_protocols
                   && CCOpt.map String.lowercase @@
                   Header.get headers "upgrade" = Some "websocket"
                   && List.mem "upgrade" @@ List.map String.lowercase @@
                   C.Header.get_multi headers "connection"
                   && Header.get headers "sec-websocket-accept" =
                      Some (nonce ^ websocket_uuid |> b64_encoded_sha1sum)
                  )
      then Lwt.fail_with "Protocol error"
      else Lwt_log.info_f ~section "Connected to %s" (Uri.to_string uri)
    in
    (try%lwt
      drain_handshake ()
     with exn ->
       safe_close ic >>= fun () ->
       Lwt.fail exn)
    >>= fun () ->
    Lwt.return (ic, oc)
  in
  connect () >|= fun (ic, oc) ->
  let read_frame = make_read_frame ~masked:true (ic, oc) in
  read_frame, send_frame ~masked:true oc

let establish_server ?timeout ?stop ~ctx ~mode react =
  let id = ref @@ -1 in
  let server_fun id (ic, oc) =
    (CU.Request.read ic >>= function
      | `Ok r -> Lwt.return r
      | `Eof ->
        (* Remote endpoint closed connection. No further action necessary here. *)
        Lwt_log.info ~section "Remote endpoint closed connection" >>= fun () ->
        Lwt.fail End_of_file
      | `Invalid reason ->
        Lwt_log.info_f ~section "Invalid input from remote endpoint: %s" reason >>= fun () ->
        Lwt.fail @@ Failure reason) >>= fun request ->
    let meth    = C.Request.meth request in
    let version = C.Request.version request in
    let uri     = C.Request.uri request in
    let headers = C.Request.headers request in
    if not (
        version = `HTTP_1_1
        && meth = `GET
        && CCOpt.map String.lowercase @@
        C.Header.get headers "upgrade" = Some "websocket"
        && List.mem "upgrade"
        @@ List.map String.lowercase @@ C.Header.get_multi headers "connection"
      )
    then Lwt.fail_with "Protocol error"
    else Lwt.return_unit >>= fun () ->
    let key = CCOpt.get_exn @@ C.Header.get headers "sec-websocket-key" in
    let hash = key ^ websocket_uuid |> b64_encoded_sha1sum in
    let response_headers = C.Header.of_list
        ["Upgrade", "websocket";
         "Connection", "Upgrade";
         "Sec-WebSocket-Accept", hash] in
    let response = C.Response.make
        ~status:`Switching_protocols
        ~encoding:C.Transfer.Unknown
        ~headers:response_headers () in
    CU.Response.write (fun writer -> Lwt.return_unit) response oc >>= fun () ->
    let send_frame = send_frame ~masked:false oc in
    let read_frame = make_read_frame ~masked:false (ic, oc) in
    react id uri read_frame send_frame
  in
  Lwt.async_exception_hook :=
    (fun exn -> Lwt_log.ign_warning ~section ~exn "async_exn_hook");
  Conduit_lwt_unix.serve ?timeout ?stop ~ctx ~mode
    (fun flow ic oc ->
       (try%lwt
         set_tcp_nodelay flow;
          incr id;
          server_fun !id (ic,oc)
        with
        | End_of_file ->
          Lwt_log.info ~section "Client closed connection"
        | Exit ->
          Lwt_log.info ~section "Server closed connection"
        | exn -> Lwt.fail exn
       ) [%finally safe_close ic]
    )

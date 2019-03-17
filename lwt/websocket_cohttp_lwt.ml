(*
 * Copyright (c) 2016-2018 Maciej Wos <maciej.wos@gmail.com>
 * Copyright (c) 2012-2018 Vincent Bernardoff <vb@luminar.eu.org>
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

open Lwt.Infix
open Websocket
module Lwt_IO = Websocket.Make(Cohttp_lwt_unix.IO)

let send_frames stream oc =
  let buf = Buffer.create 128 in
  let send_frame fr =
    Buffer.clear buf;
    Lwt_IO.write_frame_to_buf ~mode:Server buf fr;
    Lwt_io.write oc @@ Buffer.contents buf
  in
  Lwt_stream.iter_s send_frame stream

let read_frames ic oc handler_fn =
  let read_frame = Lwt_IO.make_read_frame ~mode:Server ic oc in
  let rec inner () =
    read_frame () >>=
    Lwt.wrap1 handler_fn >>=
    inner
  in inner ()

let upgrade_connection request incoming_handler =
  let headers = Cohttp.Request.headers request in
  begin match Cohttp.Header.get headers "sec-websocket-key" with
  | None ->
    Lwt.fail_invalid_arg
      "upgrade_connection: missing header `sec-websocket-key`"
  | Some key -> Lwt.return key
  end >>= fun key ->
  let hash = b64_encoded_sha1sum (key ^ websocket_uuid) in
  let response_headers =
    Cohttp.Header.of_list
      ["Upgrade", "websocket"
      ;"Connection", "Upgrade"
      ;"Sec-WebSocket-Accept", hash]
  in
  let resp =
    Cohttp.Response.make
      ~status:`Switching_protocols
      ~encoding:Cohttp.Transfer.Unknown
      ~headers:response_headers
      ~flush:true
      ()
  in
  let frames_out_stream, frames_out_fn = Lwt_stream.create () in
  let f ic oc =
    Lwt.join [
      (* input: data from the client is read from the input channel
       * of the tcp connection; pass it to handler function *)
      read_frames ic oc incoming_handler;
      (* output: data for the client is written to the output
       * channel of the tcp connection *)
      send_frames frames_out_stream oc;
    ] in
  Lwt.return (`Expert (resp, f), frames_out_fn)

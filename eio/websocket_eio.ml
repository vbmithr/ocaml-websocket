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
open Websocket
module Ws_io = Io

let send_frames stream (oc : Eio.Buf_write.t) =
  let rec send_frame stream =
    let fr = Eio.Stream.take stream in
    Ws_io.write_frame_to_buf ~mode:Server oc fr ;
    send_frame stream in
  send_frame stream

let read_frames ic oc handler_fn : unit =
  let read_frame = Ws_io.make_read_frame ~mode:Server ic oc in
  let rec inner () =
    handler_fn @@ read_frame () ;
    inner () in
  inner ()

let upgrade_connection (request : Cohttp_eio.Server.request) incoming_handler =
  let request, buf, _ = request in
  let headers = Http.Request.headers request in
  let key =
    match Http.Header.get headers "sec-websocket-key" with
    | None ->
        invalid_arg "upgrade_connection: missing header `sec-websocket-key`"
    | Some key -> key in
  let hash = b64_encoded_sha1sum (key ^ websocket_uuid) in
  let response_headers =
    Http.Header.of_list
      [ ("Upgrade", "websocket"); ("Connection", "Upgrade");
        ("Sec-WebSocket-Accept", hash) ] in
  let frames_out_stream = Eio.Stream.create max_int in
  let frames_out_fn v = Eio.Stream.add frames_out_stream v in
  let f (oc : Eio.Buf_write.t) =
    Eio.Fiber.both
      (* output: data for the client is written to the output
       * channel of the tcp connection *)
        (fun () -> send_frames frames_out_stream oc )
      (* input: data from the client is read from the input channel
       * of the tcp connection; pass it to handler function *)
        (fun () -> read_frames buf oc incoming_handler ) in
  let resp : Cohttp_eio.Server.response =
    ( Http.Response.make ~status:`Switching_protocols ~version:`HTTP_1_1
        ~headers:response_headers (),
      Cohttp_eio.Body.(Custom f) ) in
  (resp, frames_out_fn)

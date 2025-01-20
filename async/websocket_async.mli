(*
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

(** Module [Websocket_async]: websocket library for Async *)

(** This module implements a websocket client and server library in
    the spirit of the otherwise similar TCP functions of the [Lwt_io]
    module. The websocket protocol add message framing in addition of
    simple TCP communication, and this library implement framing and
    unframing of messages.
*)

open Core
open Async
module Frame = Websocket.Frame

val client :
  ?name:string ->
  ?extra_headers:Cohttp.Header.t ->
  ?random_string:(int -> string) ->
  ?initialized:unit Ivar.t ->
  app_to_ws:Frame.t Pipe.Reader.t ->
  ws_to_app:Frame.t Pipe.Writer.t ->
  net_to_ws:Reader.t ->
  ws_to_net:Writer.t ->
  Uri.t ->
  unit Deferred.Or_error.t

val client_ez :
  ?opcode:Frame.Opcode.t ->
  ?name:string ->
  ?extra_headers:Cohttp.Header.t ->
  ?heartbeat:Time_ns.Span.t ->
  ?random_string:(int -> string) ->
  Uri.t ->
  Reader.t ->
  Writer.t ->
  string Pipe.Reader.t * string Pipe.Writer.t

val server :
  ?name:string ->
  ?check_request:(Cohttp.Request.t -> bool Deferred.t) ->
  ?select_protocol:(string -> string option) ->
  ?max_frame_length:int ->
  reader:Reader.t ->
  writer:Writer.t ->
  app_to_ws:Frame.t Pipe.Reader.t ->
  ws_to_app:Frame.t Pipe.Writer.t ->
  unit ->
  unit Deferred.Or_error.t
(** [server ?request_cb ?max_frame_length reader writer app_to_ws
    ws_to_app ()] returns a thread that expects a websocket client
    connected to [reader]/[writer] and, after performing the
    handshake, will resp. read outgoing frames from [app_to_ws] and
    write incoming frames to [ws_to_app]. The thread is determined if
    any of [reader], [writer], [app_to_ws], [ws_to_app] is closed. If
    case of an error, [app_to_ws] and [ws_to_app] will be closed. Upon
    reception of the client HTTP request, [request_cb] will be called
    with the request as its argument. If [request_cb] returns true,
    the connection will proceed, otherwise, the result is immediately
    determined to [Error Exit]. If [max_frame_length] is specified and
    the server receives a frame above this size, the connection is closed. *)

val upgrade_connection :
  ?select_protocol:(string -> string option) ->
  ?ping_interval:Core.Time_ns.Span.t ->
  ?max_frame_length:int ->
  app_to_ws:Frame.t Pipe.Reader.t ->
  ws_to_app:Frame.t Pipe.Writer.t ->
  f:(unit -> unit Deferred.t) ->
  Cohttp.Request.t ->
  Cohttp.Response.t * (Reader.t -> Writer.t -> unit Deferred.t)
(** [upgrade_connection ?select_protocol ?ping_interval ?max_frame_length
    app_to_ws ws_to_app f request] returns a {!Cohttp_async.Server.response_action}.

    Just wrap the return value of this function with [`Expert].
    You can combine responses both of HTTP [`Response] handler and Websocket [`Expert] handler.

    Your handler will look like this:

    {[
    let response =
      let app_to_ws, ws_write = Pipe.create () in
      let ws_read, ws_to_app = Pipe.create () in
      Websocket_async.upgrade_connection request ~app_to_ws ~ws_to_app ~f:begin fun () ->
        let rec loop () =
          let open Websocket in
          match%bind Pipe.read ws_read with
          | `Eof -> return ()
          | `Ok ({ Frame.opcode; content; _ } as frame) ->
            let open Frame in
            let frame', closed =
              match opcode with
              | Opcode.Ping -> Some (create ~opcode:Opcode.Pong ~content ()), false
              | Opcode.Close ->
                (* Immediately echo and pass this last message to the user *)
                if String.length content >= 2 then
                  Some (create ~opcode:Opcode.Close
                          ~content:(String.sub content ~pos:0 ~len:2) ()), true
                else
                  Some (close 100), true
              | Opcode.Pong -> None, false
              | Opcode.Text
              | Opcode.Binary -> Some frame, false
              | _ -> Some (close 1002), false
            in
            begin
              match frame' with
              | None       -> Deferred.unit
              | Some frame -> Pipe.write ws_write frame
            end >>= fun () ->
            if closed
            then Deferred.unit
            else loop ()
        in
        loop ()
      end
    in
    return (`Expert response)
    ]}
*)

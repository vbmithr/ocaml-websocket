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

(** This module implements a websocket client and server library in
    the spirit of the otherwise similar TCP functions of the [Lwt_io]
    module. The websocket protocol add message framing in addition of
    simple TCP communication, and this library implement framing and
    unframing of messages. *)

include Websocket.S
  with type 'a IO.t := 'a Cohttp_lwt_unix.IO.t
   and type IO.ic := Cohttp_lwt_unix.IO.ic
   and type IO.oc := Cohttp_lwt_unix.IO.oc

val with_connection :
  ?extra_headers:Cohttp.Header.t ->
  ?random_string:(int -> string) ->
  ?ctx:Conduit_lwt_unix.ctx ->
  Conduit_lwt_unix.client ->
  Uri.t ->
  ((unit -> Websocket.Frame.t Lwt.t) * (Websocket.Frame.t -> unit Lwt.t)) Lwt.t

val establish_server :
  ?read_buf:Buffer.t ->
  ?write_buf:Buffer.t ->
  ?timeout:int ->
  ?stop:unit Lwt.t ->
  ?on_exn:(exn -> unit) ->
  ?check_request:(Cohttp.Request.t -> bool) ->
  ?ctx:Conduit_lwt_unix.ctx ->
  mode:Conduit_lwt_unix.server ->
  (Connected_client.t -> unit Lwt.t) ->
  unit Lwt.t
(** [exception_handler] defaults to [Lwt.async_exception_hook]
    [check_request] is called before the http upgrade. If it returns false, the
    websocket connection is aborted with a "403 Forbidden" response. It
    defaults to {!check_origin_with_host} *)

(** {2 Convenience functions} *)

val mk_frame_stream : (unit -> Websocket.Frame.t Lwt.t) -> Websocket.Frame.t Lwt_stream.t
(** [mk_frame_stream f] is a stream build from [f], which role is to
    receive the frames that will form the stream. When a Close frame
    is received, the stream will be closed. *)

val establish_standard_server :
  ?read_buf:Buffer.t ->
  ?write_buf:Buffer.t ->
  ?timeout:int ->
  ?stop:unit Lwt.t ->
  ?on_exn:(exn -> unit) ->
  ?check_request:(Cohttp.Request.t -> bool) ->
  ?ctx:Conduit_lwt_unix.ctx ->
  mode:Conduit_lwt_unix.server ->
  (Connected_client.t -> unit Lwt.t) ->
  unit Lwt.t
(** [establish_standard_server] is like {!establish_server} but with
    automatic handling of some frames:

    - A Pong frame is sent in response to a Ping frame,
    - a Close frame is sent in response to a Close frame.

    All frames are then passed to the frame handling function.
*)


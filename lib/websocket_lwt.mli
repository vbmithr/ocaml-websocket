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

(** Module [Websocket]: websocket library for Lwt *)

(** This module implements a websocket client and server library in
    the spirit of the otherwise similar TCP functions of the [Lwt_io]
    module. The websocket protocol add message framing in addition of
    simple TCP communication, and this library implement framing and
    unframing of messages.
*)

val section : Lwt_log_core.section

module Frame : module type of Websocket.Frame

val with_connection :
  ?extra_headers:Cohttp.Header.t ->
  ?g:Nocrypto.Rng.g ->
  ctx:Conduit_lwt_unix.ctx ->
  Conduit_lwt_unix.client ->
  Uri.t ->
  ((unit -> Frame.t Lwt.t) * (Frame.t -> unit Lwt.t)) Lwt.t

val establish_server :
  ?timeout:int ->
  ?stop:unit Lwt.t ->
  ?g:Nocrypto.Rng.g ->
  ctx:Conduit_lwt_unix.ctx ->
  mode:Conduit_lwt_unix.server ->
  (int ->
   Cohttp.Request.t ->
   (unit -> Frame.t Lwt.t) -> (Frame.t -> unit Lwt.t) -> unit Lwt.t) ->
  unit Lwt.t

(** {2 Convenience functions} *)

val mk_frame_stream : (unit -> Frame.t Lwt.t) -> Frame.t Lwt_stream.t
(** [mk_frame_stream f] is a stream build from [f], which role is to
    receive the frames that will form the stream. When a Close frame
    is received, the stream will be closed. *)

val establish_standard_server :
  ?timeout:int ->
  ?stop:unit Lwt.t ->
  ?g:Nocrypto.Rng.g ->
  ctx:Conduit_lwt_unix.ctx ->
  mode:Conduit_lwt_unix.server ->
  (int ->
   Cohttp.Request.t ->
     (unit -> Frame.t Lwt.t) -> (Frame.t -> unit Lwt.t) -> unit Lwt.t) ->
  unit Lwt.t
(** [establish_standard_server] is like {!establish_server} but with
    automatic handling of some frames:

    - A Pong frame is sent in response to a Ping frame,
    - a Close frame is sent in response to a Close frame.

    All frames are then passed to the frame handling function.
*)

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

val upgrade_connection:
  Cohttp.Request.t ->
  Conduit_lwt_unix.flow ->
  (Frame.t -> unit) ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t * (Frame.t option -> unit)) Lwt.t
(** [upgrade_connection req conn incoming_handler] takes [req], a
    connection request, [conn] a conduit, and [incoming_handler], a
    function that will process incoming websocket frames, and returns
    ([resp], [body], [push_frame]) where [resp] and [body] are used to
    respond to the client and [push_frame] is used to send websocket
    frames to the client. *)

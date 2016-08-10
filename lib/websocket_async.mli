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

(** Module [Websocket_async]: websocket library for Async *)

(** This module implements a websocket client and server library in
    the spirit of the otherwise similar TCP functions of the [Lwt_io]
    module. The websocket protocol add message framing in addition of
    simple TCP communication, and this library implement framing and
    unframing of messages.
*)

open Core.Std
open Async.Std

module Frame : module type of Websocket.Frame

val client :
  ?log:Log.t ->
  ?name:string ->
  ?extra_headers:Cohttp.Header.t ->
  ?random_string:Rng.t ->
  ?initialized:unit Ivar.t ->
  app_to_ws:(Frame.t Pipe.Reader.t) ->
  ws_to_app:(Frame.t Pipe.Writer.t) ->
  net_to_ws:Reader.t ->
  ws_to_net:Writer.t ->
  Uri.t ->
  unit Deferred.Or_error.t

val client_ez :
  ?opcode:Frame.Opcode.t ->
  ?log:Log.t ->
  ?name:string ->
  ?extra_headers:Cohttp.Header.t ->
  ?wait_for_pong:Time.Span.t ->
  ?heartbeat:Time.Span.t ->
  ?random_string:Rng.t ->
  Uri.t ->
  ('a, 'b) Socket.t ->
  Reader.t ->
  Writer.t ->
  string Pipe.Reader.t * string Pipe.Writer.t

val server :
  ?log:Log.t ->
  ?name:string ->
  ?random_string:Rng.t ->
  app_to_ws:(Frame.t Pipe.Reader.t) ->
  ws_to_app:(Frame.t Pipe.Writer.t) ->
  reader:(Reader.t) ->
  writer:(Writer.t) ->
  Socket.Address.t ->
  unit Deferred.t

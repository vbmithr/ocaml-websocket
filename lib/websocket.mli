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

module Frame : sig
  module Opcode : sig
    type t =
      | Continuation
      | Text
      | Binary
      | Close
      | Ping
      | Pong
      | Ctrl of int
      | Nonctrl of int [@@deriving show,enum]

    val is_ctrl : t -> bool
    (** [is_ctrl opcode] is [true] if [opcode] is a control
        frame. *)
  end
  (** Type representing websocket opcodes *)

  type t = { opcode    : Opcode.t [@default Opcode.Text];
             extension : int [@default 0];
             final     : bool [@default true];
             content   : string [@default ""];
           } [@@deriving show,create]
  (** The type representing websocket frames *)

  val close : int -> t
end

val with_connection :
  ?tls_authenticator:X509_lwt.authenticator ->
  ?extra_headers:((string * string) list) ->
  Uri.t ->
  ((unit -> Frame.t Lwt.t) * (Frame.t -> unit Lwt.t)) Lwt.t

type server

val establish_server :
  ?certificate:X509_lwt.priv ->
  ?buffer_size:int ->
  ?backlog:int ->
  Unix.sockaddr ->
  (int -> Uri.t ->
   (unit -> Frame.t Lwt.t) ->
   (Frame.t -> unit Lwt.t) -> unit Lwt.t) ->
  server

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

(** Module [Websocket]: websocket library for Lwt *)

(** This module implements a websocket client and server library in
    the spirit of the otherwise similar TCP functions of the [Lwt_io]
    module. The websocket protocol add message framing in addition of
    simple TCP communication, and this library implement framing and
    unframing of messages, using Lwt streams. Thus the communicating
    with the websocket server (or client) is done using an Lwt stream
    and corresponding push function.
*)

module Frame : sig
  type opcode =
    [ `Continuation
    | `Text
    | `Binary
    | `Close
    | `Close_status of int
    | `Ping
    | `Pong
    | `Ctrl of int
    | `Nonctrl of int
    ]
  (** Type representing websocket opcodes *)

  type t
  (** The type representing websocket frames *)

  (** Accessors for type t *)

  val opcode    : t -> opcode
  val extension : t -> int
  val final     : t -> bool
  val content   : t -> string

  val of_string : ?opcode:opcode -> ?extension:int -> ?final:bool -> string -> t
  (** Frame creation *)
end

val open_connection :
  ?tls_authenticator:X509_lwt.authenticator ->
  ?extra_headers:((string * string) list) -> Uri.t ->
  (Frame.t Lwt_stream.t * (Frame.t option -> unit)) Lwt.t
(** [open_connection ~tls_authenticator uri] will open a connection
    (using TLS with [~authenticator] if provided) to the given uri,
    and return a stream and a push function that can be used to send
    and receive websocket messages. *)

val with_connection :
  ?tls_authenticator:X509_lwt.authenticator ->
  ?extra_headers:((string * string) list) -> Uri.t ->
  (Frame.t Lwt_stream.t * (Frame.t option -> unit) -> 'a Lwt.t) -> 'a Lwt.t
(** Same as above except for here you provide a function that will be
    in charge of communicating with the other end, and that takes a
    stream and a push function as arguments. *)

val establish_server :
  ?certificate:X509_lwt.priv ->
  ?buffer_size:int ->
  ?backlog:int ->
  Unix.sockaddr ->
  (Uri.t -> Frame.t Lwt_stream.t * (Frame.t option -> unit) -> unit Lwt.t) -> Lwt_io_ext.server
(** Function in the spirit of [Lwt_io.establish_server], except that
    the provided function takes a stream and a push function instead
    of two channels.  Beware that when the Lwt thread returned by this
    function terminates, no more data will be read from or written to
    the socket.  Please refer to the [Lwt_io] doc for more
    information. *)

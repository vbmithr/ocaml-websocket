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

val websocket_uuid : string
val b64_encoded_sha1sum : string -> string
val upgrade_present : Cohttp.Header.t -> bool

exception Protocol_error of string

module Rng : sig
  val init : unit -> int -> string
  (** [init ?state ()] is a function that returns a string of random
      bytes of length equal to its argument. *)
end

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
      | Nonctrl of int

    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end

  type t = {
    opcode : Opcode.t;
    extension : int;
    final : bool;
    content : string;
  }

  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val create :
    ?opcode:Opcode.t ->
    ?extension:int ->
    ?final:bool ->
    ?content:string ->
    unit ->
    t

  val close : int -> t
end

val check_origin :
  ?origin_mandatory:bool -> hosts:string list -> Cohttp.Request.t -> bool
(** [check_origin ~hosts req] will return [true] if the origin header
    exists and matches one of the provided hostnames.
    If origin header is not present, return [not origin_mandatory].
    Default value of [origin_mandatory] is false.
    If origin header is present but does not contain a hostname,
    return [false].
    Hostnames in [hosts] are (ascii-)lowercased when compared.*)

val check_origin_with_host : Cohttp.Request.t -> bool
(** [check_origin_with_host] returns false if the origin header exists and its
    host doesn't match the host header *)

module type S = sig
  module IO : Cohttp.S.IO

  type mode = Client of (int -> string) | Server

  val make_read_frame :
    ?max_len:int ->
    ?buf:Buffer.t -> mode:mode -> IO.ic -> IO.oc -> unit -> Frame.t IO.t

  val write_frame_to_buf : mode:mode -> Buffer.t -> Frame.t -> unit

  module Request :
    Cohttp.S.Http_io
      with type t = Cohttp.Request.t
       and type 'a IO.t = 'a IO.t
       and type IO.ic = IO.ic
       and type IO.oc = IO.oc

  module Response :
    Cohttp.S.Http_io
      with type t = Cohttp.Response.t
       and type 'a IO.t = 'a IO.t
       and type IO.ic = IO.ic
       and type IO.oc = IO.oc

  module Connected_client : sig
    type t

    val create :
      ?max_len:int ->
      ?read_buf:Buffer.t ->
      ?write_buf:Buffer.t ->
      Cohttp.Request.t ->
      Conduit.endp ->
      IO.ic ->
      IO.oc ->
      t

    val make_standard : t -> t
    val send : t -> Frame.t -> unit IO.t
    val send_multiple : t -> Frame.t list -> unit IO.t
    val recv : t -> Frame.t IO.t
    val http_request : t -> Cohttp.Request.t
    val source : t -> Conduit.endp
  end
end

module Make (IO : Cohttp.S.IO) :
  S with type 'a IO.t = 'a IO.t and type IO.ic = IO.ic and type IO.oc = IO.oc

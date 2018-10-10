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

(** Module [Websocket_lwt]: Code common to all specialiwed Lwt
    implementations. *)

module type S = sig
  module IO : Cohttp.S.IO

  module Request : Cohttp.S.Http_io
    with type t = Cohttp.Request.t
     and type 'a IO.t = 'a IO.t
     and type IO.ic = IO.ic
     and type IO.oc = IO.oc

  module Response : Cohttp.S.Http_io
    with type t = Cohttp.Response.t
     and type 'a IO.t = 'a IO.t
     and type IO.ic = IO.ic
     and type IO.oc = IO.oc

  module Connected_client : sig
    type t

    val create :
      ?read_buf:Buffer.t -> ?write_buf:Buffer.t ->
      Cohttp.Request.t -> Conduit.endp -> IO.ic -> IO.oc -> t

    val make_standard : t -> t
    (** [make_standard t] enables the standard frame replies,
        e.g. responding to ping. *)

    val send : t -> Websocket.Frame.t -> unit IO.t
    val send_multiple : t -> Websocket.Frame.t list -> unit IO.t
    val recv : t -> Websocket.Frame.t IO.t

    val http_request : t -> Cohttp.Request.t
    (** [http_request] returns the http request that initialized this websocket
        connection *)

    val source : t -> Conduit.endp
    (** [source t] is the source address of [t]. *)
  end
end

module Make (IO : Cohttp.S.IO) : S
  with type 'a IO.t := 'a IO.t
   and type IO.ic := IO.ic
   and type IO.oc := IO.oc

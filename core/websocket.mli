val websocket_uuid : string
val b64_encoded_sha1sum : string -> string
val upgrade_present : Cohttp.Header.t -> bool

exception Protocol_error of string

module Option : sig
  val value : default:'a -> 'a option -> 'a
  val value_exn : 'a option -> 'a
  val value_map : default:'b -> f:('a -> 'b) -> 'a option -> 'b
  val map : f:('a -> 'b) -> 'a option -> 'b option
end

module Rng : sig
  val init : ?state:Random.State.t -> unit -> (int -> string)
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
    opcode: Opcode.t ;
    extension: int ;
    final: bool ;
    content: string ;
  }

  val create :
    ?opcode:Opcode.t ->
    ?extension:int ->
    ?final:bool ->
    ?content:string ->
    unit -> t

  val close : int -> t
  val show : t -> string
end

val check_origin :
  ?origin_mandatory: bool -> hosts:string list ->
  Cohttp.Request.t -> bool
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
  type mode =
    | Client of (int -> string)
    | Server

  val make_read_frame :
    ?buf:Buffer.t -> mode:mode -> IO.ic -> IO.oc -> (unit -> Frame.t IO.t)

  val write_frame_to_buf : mode:mode -> Buffer.t -> Frame.t -> unit
end

module IO(IO: Cohttp.S.IO) : S
  with type 'a IO.t = 'a IO.t
   and type IO.ic = IO.ic
   and type IO.oc = IO.oc


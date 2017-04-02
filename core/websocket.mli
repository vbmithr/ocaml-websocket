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
  val init : ?state:Random.State.t -> (int -> string)
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

module IO(IO: Cohttp.S.IO) : sig
  val make_read_frame :
    ?buf:Buffer.t ->
    ?random_string:(int -> string) ->
    masked:bool ->
    IO.ic -> IO.oc ->
    unit -> Frame.t IO.t

  val write_frame_to_buf :
    ?random_string:(int -> string) ->
    masked:bool ->
    Buffer.t ->
    Frame.t -> unit
end

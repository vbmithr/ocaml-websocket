(** Module [Rng]: Random string generation for Websocket *)

(** This module offers several random string generators that are conditionally
    built based on the packages available at compile time.
*)

type t = int -> string

val std : ?initialize:bool -> t
(** [std] uses the Random module from the stdlib. This is the RNG used by
    default, and the RNG is re-initialized each time by default, which is a
    side-effecting operation. To preserve purity, pass in [~initialize:false]
    when establishing the client or server. *)

#ifdef NOCRYPTO
val nocrypto : ?g:Nocrypto.Rng.g -> t
(** [nocrypto] uses Nocrypto for RNG generation. Optionally pass in a RNG
    instance via [?g]. *)
#endif

#ifdef CRYPTOKIT
val cryptokit : Cryptokit.Random.rng -> t
(** [cryptokit] uses Cryptokit for RNG generation. Requires a RNG instance. *)
#endif

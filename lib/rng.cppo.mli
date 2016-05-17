(** Module [Rng]: Random string generation for Websocket *)

(** This module offers several random string generators that are conditionally
    built based on the packages available at compile time.
*)

type t = int -> string

val std : ?state:Random.State.t -> t
(** [std] uses the Random module from the stdlib. If no state is passed in,
    then the default state is re-initialized and used. *)

#ifdef NOCRYPTO
val nocrypto : ?g:Nocrypto.Rng.g -> t
(** [nocrypto] uses Nocrypto for RNG generation. Optionally pass in a RNG
    instance via [?g]. *)
#endif

#ifdef CRYPTOKIT
val cryptokit : Cryptokit.Random.rng -> t
(** [cryptokit] uses Cryptokit for RNG generation. Requires a RNG instance. *)
#endif

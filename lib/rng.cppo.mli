type t = ?base64:bool -> int -> string

val std : ?initialize:bool -> t

#ifdef NOCRYPTO
val nocrypto : ?g:Nocrypto.Rng.g -> t
#endif

#ifdef CRYPTOKIT
val cryptokit : Cryptokit.Random.rng -> t
#endif

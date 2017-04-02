val init : ?g:Nocrypto.Rng.g -> Rng.t
(** [init] uses Nocrypto for RNG generation. Optionally pass in a RNG
    instance via [?g]. *)

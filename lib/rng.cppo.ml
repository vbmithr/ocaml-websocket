type t = int -> string

let std ?(initialize=true) =
  if initialize then Random.self_init ();
  fun size ->
  String.init size (fun _ -> Char.chr (Random.bits () land 0xFF))

#ifdef NOCRYPTO

let nocrypto ?g size = Nocrypto.Rng.generate ?g size |> Cstruct.to_string

#endif

#ifdef CRYPTOKIT

let cryptokit = Cryptokit.Random.string

#endif

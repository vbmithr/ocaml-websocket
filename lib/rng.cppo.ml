type t = int -> string

let std ?state =
  let state = match state with
  | None -> Random.self_init (); Random.get_state ()
  | Some s -> s in
  fun size ->
  String.init size (fun _ -> Char.chr (Random.State.bits state land 0xFF))

#ifdef NOCRYPTO

let nocrypto ?g size = Nocrypto.Rng.generate ?g size |> Cstruct.to_string

#endif

#ifdef CRYPTOKIT

let cryptokit = Cryptokit.Random.string

#endif

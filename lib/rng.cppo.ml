type t = ?base64:bool -> int -> string

let std ?(base64=false) size =
  String.init size (fun _ -> Char.chr (Random.bits () land 0xFF)) |>
  if base64 then fun s -> B64.encode ~pad:true s else fun s -> s

#ifdef NOCRYPTO

let nocrypto ?g =
    fun ?(base64=false) size ->
    Nocrypto.Rng.generate ?g size |>
    (if base64 then Nocrypto.Base64.encode else fun s -> s) |>
    Cstruct.to_string

#endif

#ifdef CRYPTOKIT

let cryptokit rng ?(base64=false) size =
    Cryptokit.Random.string rng size |>
    if base64
    then Cryptokit.(transform_string (Base64.encode_compact_pad ()))
    else fun s -> s

#endif

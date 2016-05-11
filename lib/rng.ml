type t = ?base64:bool -> int -> string

let std ?(base64=false) size =
  String.init size (fun _ -> Char.chr (Random.bits () land 0xFF)) |>
  if base64 then fun s -> B64.encode ~pad:true s else fun s -> s

let () =
  let b64 = B64.encode ~pad:true in
  Printf.printf "Stdlib    %s\n" (Rng.std 10 |> b64);
#ifdef NOCRYPTO
  Nocrypto_entropy_unix.initialize ();
  Printf.printf "Nocrypto  %s\n" (Rng.nocrypto 10 |> b64);
#endif
#ifdef CRYPTOKIT
  let rng = Cryptokit.Random.device_rng "/dev/urandom" in
  Printf.printf "Cryptokit %s\n" (Rng.cryptokit rng 10 |> b64);
#endif
  ()

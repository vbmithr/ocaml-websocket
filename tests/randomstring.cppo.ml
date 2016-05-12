let () =
  Printf.printf "Stdlib    %s\n" (Rng.std ~base64:true 10);
#ifdef NOCRYPTO
  Nocrypto_entropy_unix.initialize ();
  Printf.printf "Nocrypto  %s\n" (Rng.nocrypto ~base64:true 10);
#endif
#ifdef CRYPTOKIT
  let rng = Cryptokit.Random.device_rng "/dev/urandom" in
  Printf.printf "Cryptokit %s\n" (Rng.cryptokit rng ~base64:true 10);
#endif
  ()

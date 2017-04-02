let init ?g size = Nocrypto.Rng.generate ?g size |> Cstruct.to_string

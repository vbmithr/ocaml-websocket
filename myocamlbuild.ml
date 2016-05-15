let () =
  let open Ocamlbuild_plugin in
  dispatch begin fun hook ->
    Ocamlbuild_cppo.dispatcher hook;
  end

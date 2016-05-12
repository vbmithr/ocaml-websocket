#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let lwt = Env.bool "lwt"
let async = Env.bool "async"
let nocrypto = Env.bool "nocrypto"
let cryptokit = Env.bool "cryptokit"

let ocamlbuild = "ocamlbuild -use-ocamlfind -classic-display -plugin-tag 'package(cppo_ocamlbuild)'"


let setup_ocamlbuild () =
  let acc present pkg list = if present then pkg::list else list in

  (* add cryptokit universally if applicable *)
  let ocamlbuild = if cryptokit
    then ocamlbuild ^ " -tag-line '<{lib,tests}/*>:package(cryptokit)'"
    else ocamlbuild
  in

  let tag = ["package(base64)"] in

  let ocamlbuild =
    acc nocrypto "cppo_D(NOCRYPTO)" tag |>
    acc cryptokit "cppo_D(CRYPTOKIT)" |>
    String.concat "," |>
    Printf.sprintf "<lib/rng.ml*>:%s" |>
    Printf.sprintf "%s -tag-line '%s'" ocamlbuild
  in

  let ocamlbuild =
    acc nocrypto "package(nocrypto.unix),cppo_D(NOCRYPTO)" tag |>
    acc cryptokit "cppo_D(CRYPTOKIT)" |>
    String.concat "," |>
    Printf.sprintf "<tests/randomstring.*>:%s" |>
    Printf.sprintf "%s -tag-line '%s'" ocamlbuild in

  ocamlbuild

let () =
  let ocamlbuild = setup_ocamlbuild () in
  Pkg.describe "websocket" ~builder:(`Other (ocamlbuild, "_build")) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "lib/websocket";
    Pkg.lib ~exts:Exts.module_library "lib/rng";
    Pkg.lib ~cond:lwt ~exts:Exts.module_library "lib/websocket_lwt";
    Pkg.lib ~cond:async ~exts:Exts.module_library "lib/websocket_async";
    Pkg.bin ~cond:lwt ~auto:true "tests/wscat";
    Pkg.bin ~cond:async ~auto:true "tests/wscat_async";
    Pkg.bin ~cond:lwt ~auto:true "tests/reynir";
    Pkg.bin ~cond:lwt ~auto:true "tests/upgrade_connection";
    Pkg.bin ~auto:true "tests/randomstring";
  ]

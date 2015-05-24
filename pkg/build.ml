#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let lwt = Env.bool "lwt"

let () =
  Pkg.describe "websocket" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "lib/websocket";
    Pkg.lib ~cond:lwt ~exts:Exts.module_library "lib/websocket_lwt";
    Pkg.bin ~cond:lwt ~auto:true "tests/wscat";
    Pkg.bin ~cond:lwt ~auto:true "tests/reynir";
  ]

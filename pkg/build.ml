#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

#use "topfind"
#require "str"

let lwt = Env.bool "lwt"
let async = Env.bool "async"
let async_ssl = Env.bool "async_ssl"
let nocrypto = Env.bool "nocrypto"
let cryptokit = Env.bool "cryptokit"

let ocamlbuild = "ocamlbuild -use-ocamlfind -classic-display -plugin-tag 'package(cppo_ocamlbuild)'"

let generate_meta () =
  (* add in any deps *)
  let meta_req  = Buffer.create 25 in
  let lwt_req   = Buffer.create 25 in
  let async_req = Buffer.create 25 in
  if nocrypto then begin
    Buffer.add_string meta_req  "nocrypto";
    Buffer.add_string async_req "nocrypto.lwt";
    Buffer.add_string lwt_req   "nocrypto.unix";
  end;
  if cryptokit then begin
    let reqs = [meta_req; lwt_req; async_req] in
    List.iter (fun b -> Buffer.add_string b " cryptokit") reqs
  end;
  let tmpl =
    let file = open_in "pkg/META.template" in
    let fsz  = in_channel_length file in
    let contents = really_input_string file fsz in
    close_in file;
    contents
  in
  let meta =
    Str.(global_replace (regexp "<META_REQ>") (Buffer.contents meta_req)) tmpl |>
    Str.(global_replace (regexp "<LWT_REQ>") (Buffer.contents lwt_req)) |>
    Str.(global_replace (regexp "<ASYNC_REQ>") (Buffer.contents async_req))
  in

  let out = open_out "pkg/META" in
  output_string out meta;
  close_out out

let setup_ocamlbuild () =
  let acc present pkg list = if present then pkg::list else list in

  (* add cryptokit universally if applicable *)
  let ocamlbuild = if cryptokit
    then ocamlbuild ^ " -tag-line '<{lib,tests}/*>:package(cryptokit)'"
    else ocamlbuild
  in

  let ocamlbuild = if nocrypto
    then ocamlbuild ^ " -tag-line '<{lib,tests}/*>:package(nocrypto)'"
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
  generate_meta ();
  let ocamlbuild = setup_ocamlbuild () in
  Pkg.describe "websocket" ~builder:(`Other (ocamlbuild, "_build")) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "lib/websocket";
    Pkg.lib ~exts:Exts.module_library "lib/rng";
    Pkg.lib ~cond:lwt ~exts:Exts.module_library "lib/websocket_lwt";
    Pkg.lib ~cond:async ~exts:Exts.module_library "lib/websocket_async";
    Pkg.bin ~cond:lwt ~auto:true "tests/wscat";
    Pkg.bin ~cond:(async && async_ssl) ~auto:true "tests/wscat_async";
    Pkg.bin ~cond:lwt ~auto:true "tests/reynir";
    Pkg.bin ~cond:lwt ~auto:true "tests/upgrade_connection";
    Pkg.bin ~auto:true "tests/randomstring";
  ]

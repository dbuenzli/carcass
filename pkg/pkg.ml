#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let etc_dir = Conf.(key "etc-dir" fpath ~absent:(default "etc"))
let etc_config c = match Conf.build_context c with
| `Dev -> Ok ()
| `Pin | `Distrib ->
    let etc_dir = Conf.value c etc_dir in
    let config = strf "let dir = Fpath.v %S\n" etc_dir in
    OS.File.write "src/carcass_etc.ml" config

let install_etc_distrib_files () =
  let chop_etc_dir p = String.with_index_range p ~first:4 in
  let mv acc p = (Pkg.etc ~built:false p ~dst:(chop_etc_dir p)) :: acc in
  OS.File.fold (fun p acc -> p :: acc) [] ["etc"]
  >>= fun files -> Ok (List.fold_left mv [] files)

let build = Pkg.build ~pre:etc_config ()
let distrib =
  let files_to_watermark () =
    let not_etc f = not (String.is_prefix "etc" f) in
    Pkg.files_to_watermark ()
    >>= fun files -> Ok (List.filter not_etc files)
  in
  Pkg.distrib ~files_to_watermark ()

let () =
  Pkg.describe "carcass" ~build ~distrib @@ fun c ->
  install_etc_distrib_files () >>| fun etc_distrib_files ->
  [
    Pkg.mllib ~api:["Carcass"] "src/carcass.mllib";
    Pkg.mllib ~api:["Carcass_cli"] "src/carcass_cli.mllib";
    Pkg.bin ~auto:true "src-bin/carcass_bin" ~dst:"carcass";
  ] @ etc_distrib_files

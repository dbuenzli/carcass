#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let etc_dir = Env.string "etc-dir"
let etc_config () = match Env.build with
| `Dev -> Ok ()
| `Pin | `Release ->
    let config = strf "let dir = Fpath.v %S\n" etc_dir in
    OS.File.write "src/carcass_etc.ml" config

let etc_distrib_files () =
  begin
    let chop_etc_dir p = String.sub p 4 (String.length p - 4) in
    let mv acc p = Pkg.etc ~built:false p ~dst:(chop_etc_dir p) :: acc in
    OS.Dir.fold_files (fun p acc -> Ok (p :: acc)) [] ["etc"]
    >>= fun files -> Ok (List.fold_left mv [] files)
  end
  |> Log.on_error_msg ~use:(fun () -> [])

let distrib =
  let files_to_watermark () =
    let not_etc f = not (String.is_prefix "etc" f) in
    Pkg.files_to_watermark () >>= fun files ->
    Ok (List.filter not_etc files)
  in
  Pkg.distrib ~files_to_watermark ()

let () =
  let builder = Pkg.builder ~pre:etc_config (`OCamlbuild []) in
  Pkg.describe "carcass" ~distrib ~builder ([
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/carcass";
    Pkg.lib ~exts:Exts.module_library "src/carcass_cli";
    Pkg.bin ~auto:true "src-bin/carcass_bin" ~dst:"carcass";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
  ] @ etc_distrib_files ())

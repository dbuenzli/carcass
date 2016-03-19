#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  let builder = Pkg.builder (`OCamlbuild []) in
  Pkg.describe "$(NAME,uncapitalize)" ~builder [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/$(NAME,uncapitalize)";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]

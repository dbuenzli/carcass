#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "$(NAME,uncapitalize)" @@ fun c ->
  Ok [ Pkg.mllib "src/$(NAME,uncapitalize).mllib" ]

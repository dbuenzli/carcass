(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let pp_cst_flesh ppf map =
  let pp_binding ppf (var, value) =
    let def = Carcass.([Pat.Lit value, Loc.nil], Loc.nil) in
    Fmt.pf ppf "%s %a" var Carcass.Flesh.pp_def def
  in
  Fmt.pf ppf "@[<v>%a@]" (String.Map.pp pp_binding) map

let match_bone env no_trim all_vars file bone_id =
  let trim = not no_trim in
  begin match Carcass.Bone.find env bone_id with
  | None -> R.error_msgf "bone '%a' undefined" Fpath.pp bone_id
  | Some path ->
      Carcass.Bone.of_path ~trim path bone_id >>= fun b ->
      match Carcass.Bone.content b with
      | Carcass.Bone.Binary _ ->
          R.error_msgf "bone '%a' is a binary bone" Fpath.pp bone_id
      | Carcass.Bone.Pat pat ->
          OS.File.read file >>= fun data ->
          let data = if trim then String.trim data else data in
          match Carcass.Pat.query pat data with
          | None -> Ok 1 (* no match *)
          | Some map ->
              let map =
                let is_match_var v _ = String.is_prefix "CARCASS_MATCH_" v in
                if all_vars then map else String.Map.filter is_match_var map
              in
              if String.Map.is_empty map then Ok 0 else
              (Fmt.pr "%a@." pp_cst_flesh map; Ok 0)
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let no_trim =
  let doc = "Do not trim leading and trailing white space in matched file
             and bone."
  in
  Arg.(value & flag & info ["no-trim"] ~doc)

let all_vars =
  let doc = "Output all matched variables, not only those that are prefixed
             by 'CARCASS_MATCH_'."
  in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let file =
  let doc = "The file to match against. Use '-' for standard input." in
  Arg.(required & pos 0 (some Cli.path_arg) None & info [] ~doc ~docv:"FILE")

let doc = "match a file against a bone"
let man =
  [ `S "DESCRIPTION";
    `P "The $(tname) command matches $(i,FILE) against the structure
        of the bone identified by $(i,BONE_ID).";
    `P "If the file matches, bone variables that start with 'CARCASS_MATCH_'
        are written in flesh syntax with their matched value on standard
        output; use the option $(b,--all) to output all bone variables.
        If it doesn't match, nothing is written and the tool exits with 1.";
    `P "Variables greedily match from zero to more characters of the
        file, this is .* in regexp speak.";
    `P "By default leading and trailing white space is trimmed both in the
        file and in the bone to avoid mismatches due to editors adding
        white space at the end of files. This can be disabled with the
        $(b,--no-trim) option.";
    `S "EXAMPLES";
    `P "Add your source header and footer to a C file:";
    `Pre "  > carcass match myfile.c content | carcass bone c/src -F - ";
    `P "Update the source header and footer of a C file that matches the
        structure of the bone 'c/src':";
    `Pre "  > carcass match myfile.c c/src | carcass bone c/src -F - ";
    `P "Same as previous example but also override the COPYRIGHT_YEAR
        variable:";
    `Pre "  > carcass match myfile.c c/src | \\\\
     carcass bone c/src -F - copyright_year 2010-2016";
  ] @ Cli.common_man @ [
    `S "EXIT STATUS";
    `P "The $(tname) command exits with one of the following values:";
    `I ("0", "the file matched the bone.");
    `I ("1", "the file did not match the bone.");
    `I (">1", "an error occured.");
  ] @ Cli.see_also_main_lookup_man

let cmd =
  let info = Term.info "match" ~sdocs:Cli.common_opts ~doc ~man in
  let env = Carcass_cli.env ~docs:Cli.common_opts () in
  let t = Term.(pure match_bone $ Cli.setup env $ no_trim $ all_vars $
                file $ Cli.bone_id ~pos:1)
  in
  (t, info)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner
open Astring
open Rresult

let pp_var_id = Fmt.(quote ~mark:"'" string)

(* Argument converters *)

let path_arg =
  let parse s = match Fpath.of_string s with
  | None -> `Error (strf "%a: not a path" String.dump s)
  | Some s -> `Ok s
  in
  parse, Fpath.pp

let rec parse_cli_flesh acc = function
| id :: def :: defs ->
    let src = Carcass.Loc.Cli in
    begin match Carcass.Pat.of_input ~flesh:true ~src (`String def) with
    | Ok pat -> parse_cli_flesh ((id, pat) :: acc) defs
    | Error (`Carcass_parse (e, _)) ->
        `Error (true,
                strf "@[<v>definition of %a:@,@[%a@]@]"
                  pp_var_id id Carcass.Error.pp_parse_err e)
    end
| [] ->
    `Ok (List.rev acc)
| id :: [] ->
    `Error (true, strf "last variable %a has no definition" pp_var_id id)

(* Command lines *)

let _env ?docs cli =
  let no_user_dir =
    let doc = "Do not add ~/.carcass to the carcass search path. It
               may still be added by the .carcass directory hierarchy lookup,
               use the $(b,--no-dot-dirs) option to prevent that."
    in
    Arg.(value & flag & info ["no-user-dir"] ~doc ?docs)
  in
  let no_dot_dirs =
    let doc = "Do not add .carcass directories found in current working
               directory and up to the root directory to the carcass search
               path. The user directory is still added, use the
               $(b,--no-user-dir) option to prevent that."
    in
    Arg.(value & flag & info ["no-dot-dirs"] ~doc ?docs)
  in
  let dirs =
    let doc = "Add $(docv) to the carcass search path."in
    let dirs = [] in
    Arg.(value & opt_all path_arg dirs & info ["C"; "carcass" ] ~doc
           ~docv:"DIR" ?docs)
  in
  let flesh =
    let doc = "Add flesh file $(docv) to flesh lookup." in
    let flesh_files = [] in
    Arg.(value & opt_all path_arg flesh_files & info ["F"; "flesh"] ~doc
           ~docv:"FILE" ?docs)
  in
  let env no_user_dir no_dot_dirs dirs flesh cli = match cli with
  | `Ok cli ->
      `Ok (fun () -> Carcass.Env.v ~no_user_dir ~no_dot_dirs ~dirs ~flesh ~cli)
  | `Error _ as e -> e
  in
  Term.(ret (pure env $ no_user_dir $ no_dot_dirs $ dirs $ flesh $ cli))

let env ?docs () = _env ?docs (Term.pure (`Ok []))

let env_with_cli_flesh ?docs ~pos:p =
  let flesh =
    let doc = "Bind flesh variable $(i,ID) to definition $(i,DEF).
               These optional variable bindings override variable definitions
               found in flesh files."
    in
    Arg.(value & pos_right (p - 1) string [] & info [] ~doc ~docv:"ID DEF")
  in
  _env ?docs Term.(pure (parse_cli_flesh []) $ flesh)

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
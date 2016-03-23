(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Cmdliner

(* Formatters *)

let pp_loc ~cond =
  if not cond then Fmt.nop else
  (fun ppf l -> Fmt.pf ppf "%a " Carcass.Loc.pp l)

let pp_path_loc ppf p =
  Fmt.pf ppf "%a" Carcass.Loc.pp (Carcass.Loc.for_path p)

(* Manual *)

let common_opts = "COMMON OPTIONS"

let common_opts_man =
  [ `S common_opts; `P "These options are common to all commands." ]

let common_man =
  [ `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ common_opts_man @ [
    `S "ENVIRONMENT VARIABLES"; ]

let see_also_main_man =
  [ `S "SEE ALSO";
    `P "carcass(1)" ]

let see_also_main_lookup_man =
  [ `S "SEE ALSO";
    `P "carcass(1), carcass-lookup(5)" ]

(* Converters and arguments *)

let path_arg =
  let parse s = match Fpath.of_string s with
  | Error _ -> `Error (strf "%a: not a path" String.dump s)
  | Ok s -> `Ok s
  in
  parse, Fpath.pp

let loc ~kind =
  let doc = strf "Output the %s's location on standard error." kind in
  Arg.(value & flag & info ["l"; "loc"] ~doc)

let raw =
  let doc = "Output raw definition, without variable evaluation." in
  Arg.(value & flag & info ["r"; "raw"] ~doc)

let no_prompt =
  let doc = "Do not prompt user for information if stdin is a tty, use
             default answer or, lacking a default, fail instead."
  in
  Arg.(value & flag & info ["n"; "no-prompt"] ~doc)

let dry_run =
  let doc = "Do not write files, only report paths that would be written." in
  Arg.(value & flag & info ["dry-run"] ~doc)

let force =
  let doc = "Do not prompt before overwriting files." in
  Arg.(value & flag & info [ "force" ] ~doc)

let id kind ~docv ~pos:p =
  let doc =
    strf "The %s identifier. If $(docv) is absolute or starts with ./ \
          the corresponding file path is read as the %s. If it is '-'
          the %s is read on standard input. Otherwise it is looked up
          in carcass directories." kind kind kind
  in
  Arg.(required & pos p (some path_arg) None & info [] ~doc ~docv)

let bone_id = id "bone" ~docv:"BONE_ID"
let body_id = id "body" ~docv:"BODY_ID"

(* User interaction *)

let define_vars ?ppf ?(var_docs = String.Map.empty) ~no_prompt =
  if no_prompt || not (Unix.(isatty stdin)) then (fun _ -> None) else
  fun v ->
    Some begin match String.Map.find v var_docs with
    | None   -> Carcass.Ask.pattern ?ppf "Enter value for %s: " v
    | Some d -> Carcass.Ask.pattern ?ppf "Enter value for %s (%s): " d v
    end

let user_wants_overwrite p =
  Carcass.Ask.bool ~default:false "File %a exists, overwrite ?" Fpath.pp p

(* Basic setup for every command *)

let setup env style_renderer log_level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level log_level;
  Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ());
  env ()

let setup env =
  let style_renderer =
    let env = Arg.env_var "CARCASS_COLOR" in
    Fmt_cli.style_renderer ~docs:common_opts ~env ()
  in
  let log_level =
    let env = Arg.env_var "CARCASS_VERBOSITY" in
    Logs_cli.level ~docs:common_opts ~env ()
  in
  Term.(const setup $ env $ style_renderer $ log_level)

(* Logging and error handling *)

let log_path =
  let act = function `Write -> "WRITE" | `Wrote -> "WROTE" | `Skip -> "SKIP" in
  fun a p -> Logs.app (fun m -> m ~header:(act a) "%a" Fpath.pp p)

let log_on_error ?(level = Logs.Error) ~use = function
| Ok v -> v
| Error e ->
    begin match e with
    | `Msg e ->
        Logs.msg level (fun m -> m "%a" Fmt.text e)
    | `Carcass_parse _ as e ->
        Logs.msg level (fun m -> m "%a" Carcass.Error.pp_parse e)
    | `Carcass_eval _ as e ->
        Logs.msg level (fun m -> m "%a" Carcass.Error.pp_eval e)
    end;
    use ()

let handle_error = log_on_error ~level:Logs.Error ~use:(fun _ -> 3)

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

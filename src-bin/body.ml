(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let overwrite ~no_prompt ~force = match force with
| true -> fun _ -> true
| false ->
    match no_prompt || not Unix.(isatty stdin) with
    | true -> fun p -> Cli.log_path `Skip p; false
    | false ->
        fun p ->
          if Cli.user_wants_overwrite p then true else
          (Cli.log_path `Skip p; false)

let body_raw p =
  OS.File.read p
  >>= fun bytes -> OS.File.(write dash bytes)
  >>= fun () -> Ok 0

let body_write env no_prompt dry_run force p id root =
  Carcass.Body.of_path p id
  >>= fun body -> Carcass.Flesh.of_env env
  >>= fun flesh -> Ok (Carcass.Body.var_docs body)
  >>= fun var_docs -> Ok (Cli.define_vars ~var_docs ~no_prompt)
  >>= fun undef -> Ok (Carcass.Pat.env ~undef flesh)
  >>= fun penv -> Carcass.Body.eval_paths env penv body
  >>= fun m -> Carcass.Body.eval_bones env penv m
  >>= fun m -> match dry_run with
  | true ->
      (* We could do this after eval_paths, but that way we report errors *)
      Fpath.Map.iter (fun p _ -> Cli.log_path `Write Fpath.(root // p)) m;
      Ok 0
  | false ->
      let over = overwrite ~no_prompt ~force in
      Carcass.Body.write ~wrote:(Cli.log_path `Wrote) ~over ~dst:root m
      >>= fun () -> Ok 0

let body env raw loc no_prompt dry_run force id root =
  begin match Carcass.Body.find env id with
  | None -> R.error_msgf "body '%a' undefined" Fpath.pp id
  | Some p ->
      if loc then Fmt.epr "%a@." Cli.pp_path_loc p;
      match raw with
      | true -> body_raw p
      | false ->
          match root with
          | None -> R.error_msgf "no destination directory specified"
          | Some root -> body_write env no_prompt dry_run force p id root
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let root =
  let doc = "Root destination directory of the body. Mandatory, can only be
             ommited in raw mode (see $(b,--raw))."
  in
  Arg.(value & pos 1 (some Cli.path_arg) None & info [] ~doc ~docv:"DEST")

let doc = "output a body (file hierarchy)"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command evaluates and write the file hierarchy
        of the body identified by $(i,BODY_ID) relative to the root
        directory $(i,DEST).";
    `P "See carcass-lookup(5) for more information on the body lookup
        procedure.";
    `S "EXAMPLES";
    `P "In the current directory, create a C compilation unit u with
        your copyright information:";
    `Pre "  > carcass body c/unit . name u";
    `P "Same as the previous example but the NAME variable, corresponding
        to the name of the compilation unit, will be asked interactively:";
    `Pre "  > carcass body c/unit .";
    `P "Output the raw definition and location (on standard error) of the
        c/unit body:";
    `Pre "  > carcass body -r -l c/unit";
  ] @ Cli.common_man @ Cli.see_also_main_lookup_man

let cmd =
  let info = Term.info "body" ~sdocs:Cli.common_opts ~doc ~man in
  let env = Carcass_cli.env_with_cli_flesh ~docs:Cli.common_opts ~pos:2 in
  let t = Term.(pure body $ Cli.setup env $ Cli.raw $ Cli.loc ~kind:"body" $
                Cli.no_prompt $ Cli.dry_run $ Cli.force $ Cli.body_id ~pos:0 $
                root)
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
(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Single variable output *)

let err_undef var = R.error_msgf "variable '%a' undefined" String.pp var

let var_raw loc flesh var = match String.Map.find var flesh with
| None -> err_undef var
| Some (_, l as pat) ->
    if loc then Fmt.epr "%a@." Carcass.Loc.pp l;
    Fmt.pr "%a@." Carcass.Flesh.pp_def pat;
    Ok 0

let var_eval loc flesh var =
  let env = Carcass.Pat.env flesh in
  match Carcass.Pat.env_var_value env var with
  | None -> err_undef var
  | Some ret ->
      ret >>= fun (value, l) ->
      if loc then Fmt.epr "%a@." Carcass.Loc.pp l;
      Fmt.pr "%s@." value;
      Ok 0

let flesh_var raw loc flesh var = match raw with
| true -> var_raw loc flesh var
| false -> var_eval loc flesh var

(* Multiple variable output (in flesh syntax) *)

let pp_binding loc ppf (var, (_, l as def)) =
  Fmt.pf ppf "%a%s %a@," (Cli.pp_loc ~cond:loc) l var Carcass.Flesh.pp_def def

let eval_pp_binding loc env ppf (var, _) =
  match Carcass.Pat.env_var_value env var with
  | None -> assert false
  | Some res ->
      begin
        res >>| fun (value, l) ->
        pp_binding loc ppf (var, Carcass.Pat.([Lit value, l], l))
      end
      |> Cli.log_on_error ~use:(fun _ -> ())

let vars_raw loc flesh prefix =
  let vars = String.Map.filter (fun v _ -> String.is_prefix prefix v) flesh in
  if not (String.Map.is_empty vars)
  then Fmt.pr "@[<v>%a@]@." (String.Map.pp ~sep:Fmt.nop (pp_binding loc)) vars;
  Ok 0

let vars_eval loc flesh prefix =
  let env = Carcass.Pat.env flesh in
  let vars = String.Map.filter (fun v _ -> String.is_prefix prefix v) flesh in
  if not (String.Map.is_empty vars)
  then Fmt.pr "@[<v>%a@]@."
      (String.Map.pp ~sep:Fmt.nop (eval_pp_binding loc env)) vars;
  Ok 0

let flesh_vars raw loc flesh prefix = match raw with
| true -> vars_raw loc flesh prefix
| false -> vars_eval loc flesh prefix

(* Flesh command *)

let flesh env raw loc prefix var =
  begin
    let var = String.Ascii.uppercase var in
    Carcass.Flesh.of_env env
    >>= fun flesh -> match prefix with
    | false -> flesh_var raw loc flesh var
    | true  -> flesh_vars raw loc flesh var
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let loc =
  let doc = "Output the variable's location on standard error or, in prefix
             mode (see $(b,--prefix)), before the variable's definition."
  in
  Arg.(value & flag & info ["l"; "loc"] ~doc)

let var =
  let doc = "The variable to lookup." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"VAR")

let prefix =
  let doc = "Treat $(i,VAR) as a prefix and output in flesh syntax the
             information for all variables that match the prefix."
  in
  Arg.(value & flag & info ["p"; "prefix"] ~doc)

let doc = "output a flesh variable value"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,flesh) command evaluates and writes the value of variable
        $(i,VAR) on standard output.";
    `P "See $(mname)-lookup(5) for more information on the variable
        lookup procedure.";
    `S "EXAMPLES";
    `P "Output the value of the AUTHOR_EMAIL variable:";
    `Pre "  > carcass flesh author_email";
    `P "Output the value of the COPYRIGHT_YEAR variable in an environment
        where the CARCASS_YEAR variable is set to 1999:";
    `Pre "  > carcass flesh copyright_year carcass_year 1999";
    `P "Output the raw definition of the COPYRIGHT_YEAR variable and
        its location on standard error:";
    `Pre "  > carcass flesh -r -l copyright_year";
    `P "Output the values of variables that start with LICENSE in flesh
        syntax:";
    `Pre "  > carcass flesh -p LICENSE";
    `P "Output all variable definitions (the empty string
        is the prefix of any variable) in flesh syntax  with their location:";
    `Pre "  > carcass flesh -r -l -p \"\"";
  ] @ Cli.common_man @ Cli.see_also_main_lookup_man

let cmd =
  let info = Term.info "flesh" ~sdocs:Cli.common_opts ~doc ~man in
  let env = Carcass_cli.env_with_cli_flesh ~docs:Cli.common_opts ~pos:1 in
  let t = Term.(pure flesh $ Cli.setup env $ Cli.raw $ loc $ prefix $ var) in
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

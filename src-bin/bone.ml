(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let bone_raw p =
  OS.File.read p
  >>= fun c -> OS.File.(write dash c)
  >>= fun () -> Ok 0

let bone_eval env no_prompt p id =
  Carcass.Bone.of_path p id
  >>= fun b -> Carcass.Flesh.of_env env
  >>= fun flesh -> Ok (Cli.define_vars ~ppf:Fmt.stderr ~no_prompt)
  >>= fun undef -> Ok (Carcass.Pat.env ~undef flesh)
  >>= fun penv -> Carcass.Bone.eval penv b
  >>= fun contents -> OS.File.(write dash contents)
  >>= fun () -> Ok 0

let bone env raw loc no_prompt id =
  begin match Carcass.Bone.find env id with
  | None -> R.error_msgf "bone '%a' undefined" Fpath.pp id
  | Some p ->
      if loc then Fmt.epr "%a@." Cli.pp_path_loc p;
      match raw with
      | true -> bone_raw p
      | false -> bone_eval env no_prompt p id
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let doc = "output a bone (single file)"
let man =
  [ `S "DESCRIPTION";
    `P "The $(tname) command evaluates and writes the bone identified by
        $(i,BONE_ID) on standard output.";
    `P "See $(mname)-lookup(5) for more information on the bone lookup
        procedure.";
    `S "EXAMPLES";
    `P "Output an empty C file with your copyright information:";
    `Pre "  > carcass bone c/src";
    `P "Same as the previous example but override the
        COPYRIGHT_YEAR variable:";
    `Pre "  > carcass bone c/src copyright_year 2015-2016";
    `P "Output the raw definition and location (on standard error) of the
        c/src bone:";
    `Pre "  > carcass bone -r -l c/src";
  ] @ Cli.common_man @ Cli.see_also_main_lookup_man

let cmd =
  let info = Term.info "bone" ~sdocs:Cli.common_opts ~doc ~man in
  let env = Carcass_cli.env_with_cli_flesh ~docs:Cli.common_opts ~pos:1 in
  let t = Term.(pure bone $ Cli.setup env $ Cli.raw $ Cli.loc ~kind:"bone" $
                Cli.no_prompt $ Cli.bone_id ~pos:0)
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

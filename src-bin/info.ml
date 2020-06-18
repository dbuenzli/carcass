(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

let flesh_info env loc hidden prefix =
  let pp_loc = Cli.pp_loc ~cond:loc in
  let pp_var ppf var (pat, loc) = Fmt.pf ppf "%a%s@," pp_loc loc var in
  let pp_flesh ppf m = String.Map.iter (pp_var ppf) m in
  let keep var _ =
    String.is_prefix prefix var && (hidden || not (String.is_prefix "_" var))
  in
  Carcass.Flesh.of_env env >>= fun flesh ->
  let flesh = String.Map.filter keep flesh in
  Fmt.pr "@[<v>%a@]@?" pp_flesh flesh;
  Ok 0

let bone_info env loc hidden prefix =
  let pp_loc = Cli.pp_loc ~cond:loc in
  let pp_id = Fmt.styled `Bold Fpath.pp in
  let pp_bone ppf id p =
    Fmt.pf ppf "%a%a@," pp_loc (Carcass.Loc.for_path p) pp_id id
  in
  let pp_bones ppf m = Fpath.Map.iter (pp_bone ppf) m in
  let keep id _ = String.is_prefix prefix (Fpath.to_string id) in
  let bones = Carcass.Bone.list ~hidden env in
  let bones = Fpath.Map.filter keep bones in
  Fmt.pr "@[<v>%a@]@?" pp_bones bones;
  Ok 0

let body_info env doc loc hidden prefix =
  let pp_loc = Cli.pp_loc ~cond:loc in
  let pp_id = Fmt.styled `Bold Fpath.pp in
  let pp_body doc ppf id p =
    let path_loc = Carcass.Loc.for_path p in
    let id = Fpath.rem_ext id in
    match Carcass.Body.of_path p id with
    | Error _ as e ->
        Cli.log_on_error ~use:(fun () -> ()) e;
        Fmt.pf ppf "%a%a@," pp_loc path_loc pp_id id
    | Ok b ->
        let syn, descr = Carcass.Body.doc b in
        if not doc then begin
          Fmt.pf ppf "%a@[<1>%a@ @ %a@]@,"
            pp_loc path_loc pp_id id Fmt.text syn
        end else begin
          let pp_doc_var ppf v doc =
            Fmt.pf ppf "  @[<2>* %a@ @ @[%a@].@]@,"
              Fmt.(styled `Underline string) v Fmt.text doc
          in
          Fmt.pf ppf "@[<v>";
          if loc then Fmt.pf ppf "%a@," pp_loc path_loc;
          Fmt.pf ppf "# @[@[%a@] – @[%a@]@]@,@," pp_id id Fmt.words syn;
          Fmt.pf ppf "  @[%a@]@,@," Fmt.paragraphs descr;
          String.Map.iter (pp_doc_var ppf) (Carcass.Body.var_docs b);
          Fmt.pf ppf "@]@,"
        end
  in
  let pp_bodies doc ppf m = Fpath.Map.iter (pp_body doc ppf) m in
  let keep id _ = String.is_prefix prefix (Fpath.to_string id) in
  let bodies = Carcass.Body.list ~hidden env in
  let bodies = Fpath.Map.filter keep bodies in
  Fmt.pr "@[<v>%a@]@?" (pp_bodies doc) bodies;
  Ok 0

let env_info env =
  Fmt.pr "@[%a@]@." Fpath.pp Carcass.Env.etc_dir;
  Ok 0

let kind_info env doc loc hidden prefix kind =
  begin match kind with
  | `Flesh -> flesh_info env loc hidden prefix
  | `Bone -> bone_info env loc hidden prefix
  | `Body -> body_info env doc loc hidden prefix
  | `Env -> env_info env
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let doc_opt =
  let doc = "Show documentation of the element in the output (if available)." in
  Arg.(value & flag & info ["d"; "doc"] ~doc)

let loc =
  let doc = "Show the element's location of definition in the output." in
  Arg.(value & flag & info ["l"; "loc"] ~doc)

let hidden =
  let doc = "Show hidden elements in the output. An element is hidden
             if the last path component of its identifier starts with a '_'
             character."
  in
  Arg.(value & flag & info ["h"; "hidden"] ~doc)

let kind =
  let kind = ["flesh", `Flesh; "bone", `Bone; "bones", `Bone; "body", `Body;
              "bodies", `Body; "env", `Env;  ]
  in
  let doc =
    strf "Kind of elements to consider. $(docv) must be one of %s (singular
          and plural forms are equivalent)."
      (Arg.doc_alts_enum kind)
  in
  let kind = Arg.enum kind in
  Arg.(required & pos 0 (some kind) None & info [] ~doc ~docv:"KIND")

let prefix =
  let doc = "Only output elements whose identifier starts with $(docv)." in
  Arg.(value & pos 1 string "" & info [] ~doc ~docv:"PREFIX")

let doc = "output information about flesh variables, bones and bodies"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,info) command outputs, depending on $(i,KIND), information
        about flesh variables, bones or bodies defined in the current
        environment.";
    `P "Only information about elements whose identifier starts with $(i,PREFIX)
        is output; if unspecified this is the empty string and all elements
        are listed.";
    `S "EXAMPLES";
    `P "List all flesh variable identifiers in the current environment and
        where they are defined:";
    `Pre "  > carcass info -l flesh";
    `P "List all bone identifiers, including hidden ones, in the current
        environment and where they are defined:";
    `Pre "  > carcass info -h -l bone ";
    `P "Show documentation about bodies whose identifiers start with 'www':";
    `Pre "  > carcass info -d body www";
  ] @ Cli.common_man @ Cli.see_also_main_lookup_man

let cmd =
  let info = Term.info "info" ~sdocs:Cli.common_opts ~doc ~man in
  let env = Carcass_cli.env ~docs:Cli.common_opts () in
  let term = Term.(pure kind_info $ Cli.setup env $ doc_opt $ loc $ hidden $
                   prefix $ kind)
  in
  term, info

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

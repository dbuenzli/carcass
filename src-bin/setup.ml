(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let flesh_setup_bone = Fpath.(Carcass.Env.etc_dir / "flesh.setup")

(* Carcasses setup *)

let user_wants_carcasses () =
  Carcass.Ask.bool ~default:true
    "Do@ you@ want@ to@ install@ the@ sample@ bones@ and@ bodies ?"

let work_around_opam_install mode data =
  (* see https://github.com/ocaml/opam/issues/2430 *)
  if String.is_prefix "#!" data then 0o755 else mode

let install_carcasses dry_run force setup_dir =
  let copy src () =
    if Fpath.equal src flesh_setup_bone then () else
    let dst = match Fpath.rem_prefix (Carcass.Env.etc_dir) src with
    | None -> assert false
    | Some rel -> Fpath.(setup_dir // rel)
    in
    begin match dry_run with
    | true -> Cli.log_path `Write dst; Ok ()
    | false ->
        OS.File.exists dst >>= function
        | true when not force && not (Cli.user_wants_overwrite dst) ->
            Cli.log_path `Skip dst; Ok ()
        | _ ->
            OS.Path.Mode.get src
            >>= fun mode -> OS.File.read src
            >>= fun data -> Ok (work_around_opam_install mode data)
            >>= fun mode -> OS.Dir.create (Fpath.parent dst)
            >>= fun _ -> OS.File.write ~mode dst data
            >>= fun () -> Cli.log_path `Wrote dst; Ok ()
    end
    |> Logs.on_error_msg ~use:(fun () -> ())
  in
  if force || (user_wants_carcasses ())
  then OS.Dir.fold_contents ~elements:`Files copy () Carcass.Env.etc_dir
  else Ok ()

(* Flesh setup *)

let pr_flesh_start () =
  Fmt.pr "@\n@[%a@]@\n@." Fmt.text "Please answer the following questions."

let pr_flesh_end flesh_file =
  Fmt.pr "@\n@[Thanks,@ consult@ and@ edit@ ";
  Fmt.pr "%a" Fpath.pp flesh_file;
  Fmt.pr "@ for@ further@ adjustments.@]@\n@."

let git_conf key =
  let parse = function "" -> None | v -> Some v in
  OS.Cmd.(run_out Cmd.(v "git" % "config" % key) |> to_string) >>| parse
  |> Logs.on_error_msg ~level:Logs.Debug ~use:(fun () -> None)

let ask ~guess ~default what = match guess with
| None -> Carcass.Ask.string ~default " What@ is@ %a ? " Fmt.text what
| Some default ->
    Carcass.Ask.string ~default " What is@ %a ?@ [enter for '%s'] "
      Fmt.text what default

let ask_name () =
  ask ~guess:(git_conf "user.name") ~default:"nobody" "your name"

let ask_email () =
  ask ~guess:(git_conf "user.email") ~default:"nobody@example.org" "your email"

let ask_homepage gh_user =
  let guess = match gh_user with
  | None -> None | Some u -> Some (strf "https://%s.github.io" u)
  in
  ask ~guess ~default:"https://www.example.org" "your homepage"

let ask_pkg_homepage_root gh_username =
  let guess = match gh_username with
  | None -> None | Some u -> Some (strf "https://github.com/%s" u)
  in
  ask ~guess ~default:"https://www.example.org/software"
    "the root address for the homepage of your packages"

let user_uses_github () =
  Carcass.Ask.bool ~default:true " Are@ you@ using@ github@ ?"

let ask_github_username () =
  if not (user_uses_github ()) then None else
  Some (Carcass.Ask.string ~default:"_" " What@ is@ your@ github@ username ? ")

let setup_flesh_env name email homepage pkg_homepage_root =
  let lit s =
    let loc = Carcass.Loc.for_cli s in
    [Carcass.Pat.Lit s, loc], loc
  in
  Carcass.Pat.env @@
  String.Map.(empty
              |> add "CARCASS_SETUP_AUTHOR_HOMEPAGE" (lit homepage)
              |> add "CARCASS_SETUP_AUTHOR_NAME" (lit name)
              |> add "CARCASS_SETUP_AUTHOR_EMAIL" (lit email)
              |> add "CARCASS_SETUP_PKG_HOMEPAGE_ROOT" (lit pkg_homepage_root))

let ask_env () =
  let name = ask_name () in
  let email = ask_email () in
  let gh_username = ask_github_username () in
  let homepage = ask_homepage gh_username in
  let pkg_homepage_root = ask_pkg_homepage_root gh_username in
  Fmt.pr "@.";
  setup_flesh_env name email homepage pkg_homepage_root

let setup_flesh dry_run force setup_dir =
  let flesh_file = Fpath.(setup_dir / "flesh") in
  OS.File.exists flesh_file >>= function
  | true when not force && not (Cli.user_wants_overwrite flesh_file) -> Ok ()
  | _ ->
      Carcass.Bone.of_path flesh_setup_bone flesh_setup_bone
      >>= fun bone -> pr_flesh_start (); Ok (ask_env ())
      >>= fun env -> Carcass.Bone.eval env bone
      >>= fun flesh -> match dry_run with
      | true -> Cli.log_path `Write flesh_file; Ok ()
      | false ->
          OS.Dir.create setup_dir
          >>= fun _ -> OS.File.write flesh_file flesh
          >>| fun () -> Cli.log_path `Wrote flesh_file; pr_flesh_end flesh_file

(* Setup command *)

let pr_setup_end () =
  Fmt.pr "@\n@[%a@]@\n@." Fmt.text
    "Setup is complete. Run `carcass help basics` for a short introduction \
     to carcass."

let setup env dry_run force kind setup_dir =
  let do_flesh = kind <> `Carcass in
  let do_carcass = kind <> `Flesh in
  begin
    (match setup_dir with None -> Carcass.Env.user_dir () | Some d -> Ok d)
    >>= fun d -> (if do_flesh then setup_flesh dry_run force d else Ok ())
    >>= fun () ->
    (if do_carcass then install_carcasses dry_run force d else Ok ())
    >>= fun () -> pr_setup_end (); Ok 0
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let setup_dir =
  let doc = "The directory in which to perform the setup." in
  let dir p = match Fpath.to_string p with
  | "~/.carcass" -> None | _ -> Some p
  in
  let arg =
    Arg.(value & opt Cli.path_arg (Fpath.v "~/.carcass") &
         info ["dir"] ~doc ~docv:"DIR")
  in
  Term.(const dir $ arg)

let kind =
  let kind = Arg.enum ["all", `All; "flesh", `Flesh; "carcass", `Carcass] in
  let doc = "Kind of setup to perform. `flesh` for flesh setup, `carcass`
             for sample bones and bodies installation and `all` for both."
  in
  Arg.(value & pos 0 kind `All & info [] ~doc ~docv:"KIND")

let doc = "setup the user ~/.carcass directory"
let man =
  [ `S "DESCRIPTION";
    `P "The $(tname) command asks the user a few questions to
        write the personal ~/.carcass/flesh file and copies
        a few sample bones and bodies from carcass's etc directory to
        ~/.carcass.";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
  ]
  @ Cli.see_also_main_lookup_man

let cmd =
  let info = Term.info "setup" ~sdocs:Cli.common_opts ~doc ~man in
  let env = Carcass_cli.env ~docs:Cli.common_opts () in
  let t = Term.(pure setup $ Cli.setup env $ Cli.dry_run $
                Cli.force $ kind $ setup_dir)
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

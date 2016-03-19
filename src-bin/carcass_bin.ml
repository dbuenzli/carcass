(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner

let cmds =
  [ Setup.cmd; Flesh.cmd; Bone.cmd; Body.cmd; Info.cmd; Match.cmd; Help.cmd;]

let main _ = `Help (`Pager, None)

(* Command line interface *)

let doc = "define file and directory carcasses"
let man =
  [ `S "DESCRIPTION";
    `P "$(b,$(mname)) defines and generates file and directory structures.";
    `P "The primary aim of $(b,$(mname)) is to help programmers to quickly
        setup new software projects and deal with source and licensing
        boilerplate during program development. $(b,$(mname)) is agnostic to
        content.";
    `P "Use '$(b,$(mname)) help basics' for understanding the basics.";
    `Noblank;
    `P "Use '$(b,$(mname)) help lookup' for help about flesh, bone and \
        body lookups.";
    `Noblank;
    `P "Use '$(b,$(mname)) help syntax' for help about the syntax of
        carcass files.";
    `Noblank;
    `P "Use '$(b,$(mname)) help $(i,COMMAND)' for information about
        $(i,COMMAND).";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
    `S "BUGS";
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information.";
    `S "AUTHOR";
    `P "Daniel C. Buenzli, $(i,http://erratique.ch)";
    `S "SEE ALSO";
    `P "$(mname)-basics(7), $(mname)-lookup(5), $(mname)-syntax(5)"; ]

let main =
  let version = "%%VERSION%%" in
  let info = Term.info "carcass" ~version ~doc ~sdocs:Cli.common_opts ~man in
  let env = Carcass_cli.env ~docs:Cli.common_opts () in
  let t = Term.(ret (const main $ Cli.setup env)) in
  (t, info)

let main () = match Term.eval_choice main cmds with
| `Error _ -> exit 1
| `Ok ret when ret <> 0 -> exit ret
| _ -> if Logs.err_count () > 0 then exit 1 else exit 0

let () = main ()

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

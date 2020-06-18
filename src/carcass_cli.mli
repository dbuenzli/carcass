(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} support for [Carcass].

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}  *)

(** {1 Command lines for setting up the environment} *)

val env : ?docs:string -> unit -> (unit -> Carcass.Env.t) Cmdliner.Term.t
(** [env ()] is a {!Cmdliner} term that has all the options to setup a
    carcass environment. The closure should be called once {!Logs} has
    been setup. The options are documented under the [docs] section
    (defaults to the default in {!Cmdliner.Arg.info}). *)

val env_with_cli_flesh :
  ?docs:string -> pos:int -> (unit -> Carcass.Env.t) Cmdliner.Term.t
(** [env_with_cli_flesh ~pos] is like {!env} but also defines flesh as
    positional command line arguments starting at [pos]. *)

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

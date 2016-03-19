(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Cmdliner} and common definitions for commands. *)

open Rresult
open Astring
open Cmdliner

(** {1 Formatters} *)

val pp_loc : cond:bool -> Carcass.Loc.t Fmt.t
(** [pp_loc cond] is {!Fmt.nop} if [cond] is [false] and
    {!Carcass.Loc.pp} followed by a space otherwise. *)

val pp_path_loc : Fpath.t Fmt.t
(** [pp_path_loc] formats a location to the start position of
    the path using {!Carcass.Loc.pp}. *)

(** {1 Manual section for common options} *)

val common_opts : string
(** [common_opts] is the manual section were common options are
    documented. *)

val common_opts_man : Cmdliner.Manpage.block list
(** [common_opts_man] is the manual section for common options. *)

val common_man : Cmdliner.Manpage.block list
(** [common_man] is a manual fragment common to many commands. *)

val see_also_main_man : Cmdliner.Manpage.block list
(** [see_also_main_man] is a "see also" manpage fragment. *)

val see_also_main_lookup_man : Cmdliner.Manpage.block list
(** [see_also_main_lookup_man] is a "see also" manpage fragment. *)

(** {1 Converters and arguments} *)

val path_arg : Fpath.t Arg.converter
(** [path_arg] is a path argument converter. *)

val loc : kind:string -> bool Term.t
(** A [--loc] option to report locations for elements of kind [kind]. *)

val raw : bool Term.t
(** A [--raw] option to require raw output. *)

val no_prompt : bool Term.t
(** A [--no-prompt] option to disable human interaction if
    [stdin] is a tty. *)

val dry_run : bool Term.t
(** A [--dry-run] option to report written files without writting them. *)

val force : bool Term.t
(** A [--force] option to disable human interaction on file overwrites. *)

val bone_id : pos:int -> Fpath.t Term.t
(** A bone identifier positional argument at position [pos]. *)

val body_id : pos:int -> Fpath.t Term.t
(** A body identifier positional argument at position [pos]. *)

(** {1 User interaction} *)

val define_vars :
  ?ppf:Format.formatter ->
  ?var_docs:string String.Map.t -> no_prompt:bool ->
  (string -> (Carcass.Pat.t, Carcass.Error.parse) result option)
(** [define_vars ~var_docs ~no_prompt] is a function [f] that given a
    variable name asks the user on [ppf] to define a value for it by
    reading from standard input; but only if [stdin] is a tty and
    [no_prompt] is [false]. If [var_docs] contains a binding for the
    asked variable name it is used to document the questions, defaults
    to {!String.Map.empty}. *)

val user_wants_overwrite : Fpath.t -> bool
(** [user_wants_overwrite p] asks if [p] should be overwritten,
    and returns the answer. *)

(** {1 Basic setup for every command} *)

val setup : (unit -> Carcass.Env.t) Term.t -> Carcass.Env.t Term.t
(** [setup env] defines a basic setup common to all commands. This
    includes, by side effect, setting log verbosity for {!Logs},
    ajusting colored output and finally calling [env] to create
    a carcass environment. *)

(** {1 Logging and error handling} *)

val log_path : [`Write | `Wrote | `Skip ] -> Fpath.t -> unit
(** [log_path action p] logs action [a] for path. *)

val log_on_error :
  ?level:Logs.level ->
  use:(unit -> 'a) ->
  ('a, [< `Msg of string | Carcass.Error.parse | Carcass.Error.eval]) result ->
  'a
(** [log_on_error] is like {!Logs.on_error}. *)

val handle_error :
  (int, [< `Msg of string | Carcass.Error.parse | Carcass.Error.eval]) result ->
  int
(** [handle_error r] is [r]'s result or logs [r]'s error and returns 3. *)

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
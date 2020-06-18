(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Define file and directory carcasses. *)

(** {1 Carcass} *)

open Astring
open Rresult

(** Locating text in input data. *)
module Loc : sig

  (** {1 Positions} *)

  type pos = int * int
  (** The type for positions. A one-based line number and and a
      zero-based column number. Each Unicode scalar value on a line
      increments column number by one. *)

  val nil_pos : pos
  (** [nil_pos] is an invalid position. *)

  val zero_pos : pos
  (** [zero_pos] is [(1,0)]. *)

  (** {1 Ranges} *)

  type range = pos * pos
  (** The type for ranges. Start and stop position. *)

  val nil_range : range
  (** [nil_range] is an invalid range. *)

  val zero_range : range
  (** [zero_range] is [(zero_pos, zero_pos)]. *)

  (** {1 Locations} *)

  (** The type for location sources. *)
  type src = Builtin | Cli | File of Fpath.t

  type t = src * range
  (** The type for locations. *)

  val nil : t
  (** [nil] is an invalid builtin location. *)

  val for_builtin : string -> t
  (** [for_builtin s] is a builtin location for string [s]. *)

  val for_cli : string -> t
  (** [for_cli s] is a cli location for string [s]. *)

  val for_path : ?range:range -> Fpath.t -> t
  (** [for_path ~range p] is the range [range] (defaults to {!zero_range})
      of [p]. *)

  val pp : t Fmt.t
  (** [pp] formats locations according to
      {{:http://www.gnu.org/prep/standards/standards.html#Errors}GNU
      conventions}. *)

  (** {1 Traces} *)

  type trace = t list
  (** The type for traces. Lists of locations. *)

  val nil_trace : trace
  (** [nil] is an empty trace. *)

  val pp_trace : trace Fmt.t
  (** [pp_trace] formats traces using {!pp}. *)
end

(** Parse and evaluation errors. *)
module Error : sig

  (** {1 Parse errors} *)

  type unexpected = [ `Uchar of Uchar.t | `Lexeme of string | `Eoi ]
  (** The type for unexpected parse input. *)

  type expected =
    [ `Keyword of string | `Id | `Lpar | `Rpar | `Qmark | `Comma | `Dollar
    | `Escaped_char | `Atom ]
  (** The type for expected parse input. *)

  (** The type for parse errors cases. *)
  type parse_err =
  | Illegal_bytes of string
  | Illegal_escape of Uchar.t
  | Illegal_variable_transform of string
  | Illegal_binding_id of string
  | Unclosed of [ `Quoted_atom | `Var_ref ]
  | Unexpected of unexpected * expected list

  (** The type for parse errors. *)
  type parse = [ `Carcass_parse of parse_err * Loc.t ]

  val pp_parse_err : parse_err Fmt.t
  (** [pp_parse_err] is a formatter for parse error cases. *)

  val pp_parse : parse Fmt.t
  (** [pp_parse] is a formatter for parse errors. *)

  (** {1 Evaluation errors} *)

  type eval_id = [ `Var of string | `Bone of string | `Body of string ]
  (** The type for evaluated identifiers. *)

  (** The type for evaluation errors cases. *)
  type eval_err =
  | Circular of eval_id
  | Parse of eval_id * [ parse | R.msg ]
  | Undefined of eval_id
  | Bound_path of string * [ `Illegal | `Escapes ]

  (** The type for evaluation errors. *)
  type eval = [ `Carcass_eval of eval_err * Loc.trace ]

  val pp_eval_id : eval_id Fmt.t
  (** [pp_eval_def] is a formatter for evaluated identifiers. *)

  val pp_eval_err : eval_err Fmt.t
  (** [pp_eval_err] is a formatter for evaluation error cases. *)

  val pp_eval : eval Fmt.t
  (** [pp_eval] is a formatter for evaluation errors. *)
end

(** Patterns.

    Patterns are strings with variable references of the form
    [$(VAR[,transform])]. In patterns any literal [$] must be written
    [$$].

    See [carcass-syntax(5)] for more information. *)
module Pat : sig

  (** {1 Variable reference transforms} *)

  (** The type for variable reference transforms. *)
  type transform =
  | Uppercase | Lowercase | Capitalize | Uncapitalize | Indent of string

  val transform_to_string : transform -> string
  (** [transform_of_string t] parses a transform from [s]. *)

  val pp_transform : transform Fmt.t
  (** [pp_transform] is a pretty printer for transforms. *)

  (** {1 Patterns} *)

  (** The type for pattern lexemes. Either a string literal (where
      $ are unescaped) or a variable reference. *)
  type lexeme = Lit of string | Var of string * transform option

  type t = (lexeme * Loc.t) list * Loc.t
  (** The type for patterns. A list of localized pattern lexemes tupled
      with a location spanning the whole pattern. *)

  val empty : t
  (** [empty] is an empty builtin pattern. *)

  val dom : t -> String.set
  (** [dom p] is the set of variable references in [p]. *)

  val equal : t -> t -> bool
  (** [equal p p'] is [p = p']. *)

  val compare : t -> t -> int
  (** [compare p p'] is [Pervasives.compare p p']. *)

  val to_string : ?flesh:bool -> t -> string
  (** [to_string ~flesh p] converts [p] to a string according the carcass
      syntax for variable references. Escapes the $ in lexeme
      literals to $$.

      If [flesh] is [true] (defaults to [false]) also escapes double
      quote characters ['"'] (U+0022) with the sequence ["\\\""]
      (<U+005C, U+0022>) and backslash characters ['\\'] (U+005C) with
      the sequence ["\\\\"] (<U+005C, U+005C>). *)

  val of_input :
    ?flesh:bool ->
    src:Loc.src ->
    [ `String of string | `Channel of in_channel ] ->
    (t, [> Error.parse]) Result.result
  (** [of_input ~flesh ~src i] considers [i] as a single atom and
      returns its pattern. If [flesh] is [true] carcass escapes are
      recognized and interpreted; if [false] (defaults) only variable
      references are recognized. *)

  val pp : ?flesh:bool -> t Fmt.t
  (** [pp] formats patterns verbatim like {!to_string}. *)

  (** {1 Substitution} *)

  val subst : (string -> string option) -> t -> t
  (** [subst defs p] substitutes variables in [p] by the value they map
      to in [defs]. *)

  (** {1 Evaluation} *)

  type env
  (** The type for evaluation environments. *)

  val env :
    ?undef:(string -> (t, Error.parse) result option) -> t String.map -> env
  (** [env ~undef m] is an evaluation environment in which variables are
      defined according to the map [m]. [undef] is called on undefined
      variables; its result is cached by the environement, defaults to
      [(fun _ -> None]). *)

  val env_var_value : env -> string ->
    (string * Loc.t, [> Error.eval]) result option
  (** [env_var_value env var] is value of variable [var] in [env], if
      defined. *)

  val eval : env -> t -> (string * Loc.t, [> Error.eval]) result
  (** [eval env pat] is the evaluation of pattern [pat]
      in environment [env]. *)

  (** {1 Matching} *)

  val query : ?init:string String.map -> t -> string -> string String.map option
  (** [query ~init p s] returns an environment mapping each pattern variable
      of [p] to its matched part in [s], if [s] matches [p]. Variables are
      added to [init]. Variables greedily match from zero to more
      characters of the file, i.e. .* in regexp speak. *)
end

(** Ask values on standard input. *)
module Ask : sig

  (** {1 Asking values} *)

  type ('a, 'b) t = ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** The type for questions formatted according to ['a] and whose result is
      ['b]. *)

  val value :
    ?ppf:Format.formatter ->
    parse:(string -> ('a, 'b) Result.result) ->
    ('c, ('a, 'b) Result.result) t
  (** [value ~parse] asks for a value parsed according to [parse].
      The question is written on [ppf] (Defaults to {!Fmt.stdout}). *)

  val pattern :
    ?ppf:Format.formatter ->
    ('a, (Pat.t, [> Error.parse]) Result.result) t
  (** [pattern] asks for a pattern. *)

  val bool : ?ppf:Format.formatter -> default:bool -> ('a, bool) t
  (** [bool] asks for a boolean value. If no input is provided or
      no boolean sense can be made from the input, defaults to [default]. *)

  val string : ?ppf:Format.formatter -> default:string -> ('a, string) t
  (** [string] asks for a string value. If no input is provided
      defaults to [default]. *)
end

(** Carcass environment.

    A carcass environement defines how {{!Flesh}flesh},
    {{!Bone}bone} and {{!Body}body} lookups get resolved. *)
module Env : sig

  (** {1 Directories} *)

  val etc_dir : Fpath.t
  (** [etc_dir] is the path to the install's [etc] directory. *)

  val user_dir : unit -> (Fpath.t, [> R.msg]) result
  (** [user_dir] is the path to the user's carcass directory. *)

  (** {1 Environments} *)

  type t
  (** The type for environments. *)

  val v :
    no_user_dir:bool ->
    no_dot_dirs:bool ->
    dirs:Fpath.t list ->
    flesh:Fpath.t list ->
    cli:(string * Pat.t) list -> t
  (** [v ~no_user_dir ~no_dot_dirs ~dirs ~flesh ~cli] is an environment such
      that looks up are done, in order:
      {ul
      {- For flesh, first in [cli], followed by [flesh] files (starting from
         the last one), followed by files [d/flesh] with [d] in [dirs]
         (starting from the last one).}
      {- For bones and bodies, first in directory [dirs] (starting from
         the last one), then in [.carcass] directories from current
         directory up to root (except if [no_dot_dirs] is [true]), then
         in [~/.carcass] (except if [no_user_dir] is [true]).}} *)
end

(** Flesh (variable definitions).

    See [carcass-syntax(5)] for more information about the syntax
    of flesh files. *)
module Flesh : sig

  (** {1 Flesh} *)

  type t = Pat.t String.map
  (** The type for flesh. Maps variable names to their definition. *)

  val builtins : t
  (** [builtins] are the built-in variable definitions. *)

  val of_input :
    ?init:Pat.t String.map ->
    src:Loc.src ->
    [ `String of string | `Channel of in_channel ] ->
    (t, [> Error.parse]) Result.result
  (** [of_input ~init ~src input] reads flesh variable bindings from [input]
      and adds them to [init] (defaults to {!builtins}). *)

  val of_env :
    ?init:Pat.t String.map ->
    Env.t ->
    (t, [> Error.parse]) Result.result
  (** [of_env init env] are the variable bindings available in
      environment [env] added to [init] (defaults to {!builtins}). *)

  val pp_def : Pat.t Fmt.t
  (** [pp_def] formats a pattern like a flesh variable definition. *)

  val pp : t Fmt.t
  (** [pp] formats flesh as valid carcass flesh syntax. *)
end

(** Bones (single files). *)
module Bone : sig

  (** {1 Lookup} *)

  type id = Fpath.t
  (** The type for bone ids. *)

  val find : Env.t -> id -> Fpath.t option
  (** [find env id] finds the full path to the bone identified by [id]
      in environment [env]. If [id] is {!OS.File.dash} or an absolute path or
      starts with [./] and the path exists, [Some id] is returned. *)

  val list : ?hidden:bool -> Env.t -> Fpath.t Fpath.map
  (** [list ~hidden env] maps bone identifiers found in the environment [env]
      to their [path]. If [hidden] is [true] hidden bones (those whose
      last segment start with a ['_']) are also in the map (defaults to
      [false]). *)

  (** {1 Bones} *)

  type content =
    | Binary of string (** Binary bone. *)
    | Pat of Pat.t (** Textual bone. *)
  (** The type for bone contents. A bone is deemed binary if a null byte
      is found in its content. *)

  type t
  (** The type for bones. *)

  val id : t -> id
  (** [id b] is the bone's id. *)

  val content : t -> content
  (** [content b] is the bone's content. *)

  val is_exec : t -> bool
  (** [is_exec b] is [true] if the bone is executable. *)

  val of_input :
    ?trim:bool ->
    src:Loc.src ->
    [ `String of string ] -> is_exec:bool -> id ->
    (t, [> Error.parse]) Result.result
  (** [of_input ~src input is_exec id] reads a bone with id [id] from
      [input].  [is_exec] is the value for {!is_exec}.  If [trim] is
      [true] (defaults to [false]), a textual bone's leading and
      trailing white space is trimmed with {!String.trim}. *)

  val of_path :
    ?trim:bool -> Fpath.t -> id -> (t, [> Error.parse | `Msg of string ]) result
  (** [of_path ~trim p] reads a bone with id [id] from path [p] using
      {!of_input}. The resulting bone's {!is_exec} is [true] iff the path
      is executable for the user.  *)

  (** {1 Evaluation} *)

  val eval : Pat.env -> t -> (string, [> Error.eval]) result
  (** [eval env b] evaluates [b] to a string in the pattern evaluation
      environment [env]. *)
end

(** Bodies (file system hierarchies). *)
module Body : sig

  (** {1 Lookup} *)

  type id = Fpath.t
  (** The type for body ids. *)

  val find : Env.t -> id -> Fpath.t option
  (** [find env id] finds the full path to the bone identified by [id]
      in environment [env]. If [id] is an absolute path or starts with
      [./] and the path exists, [Some id] is returned. If it is looked up
      in the environment and [id] has no [.body] extension one is added. *)

  val list : ?hidden:bool -> Env.t -> Fpath.t Fpath.map
  (** [list ~hidden env] maps body ids found in the environment [env] to
      their [path]. If [hidden] is [true] hidden bodies (those whose
      last segment start with a ['_']) are also in the map (defaults to
      [false]). *)

  (** {1 Bodies} *)

  type binding_id = Fpath.t
  (** The type for binding ids, either a bone id or a body id. Must be
      a relative path that doesn't start with ./. *)

  type t
  (** The type for bodies *)

  val id : t -> id
  (** [id b] is the body's id. The id always has [.body] file extension. *)

  val doc : t -> string * string
  (** [doc b] is the synopsis and documentation of [b]. *)

  val var_docs : t -> string String.map
  (** [var_docs b] maps a selection of variables to a documentation
      string. *)

  val bindings : t -> (Pat.t * (binding_id * Loc.t)) list
  (** [bindings b] are the uninterpreted path bindings found in [b]. *)

  val of_input :
    src:Loc.src ->
    [ `String of string | `Channel of in_channel ] -> id ->
    (t, [> Error.parse]) Result.result
  (** [of_input ~src input id] reads a body with id [id] from [input]. *)

  val of_path : Fpath.t -> id -> (t, [> Error.parse | R.msg]) result
  (** [of_path p id] reads a body with id [id] from path [p] using
      {!of_input}. *)

  (** {1 Evaluation} *)

  val eval_paths :
    Env.t -> Pat.env -> t ->
    ((Bone.id * Loc.trace) Fpath.map, [> Error.eval | R.msg ]) result
  (** [eval_paths env penv id b] evaluates all the paths of bone [b]'s
      bindings and maps them to their bone id using [env] to lookup
      sub-bodies and [penv] to evaluate the path patterns. *)

  val eval_bones :
    Env.t -> Pat.env -> (Bone.id * Loc.trace) Fpath.map ->
    ((string * bool) Fpath.map, [> Error.eval | R.msg]) result
  (** [eval_bones env penv m] evaluates all the bone ids in the
      path map [m] to their content and executable status using [env] to
      look them up and [penv] to evaluate textual bones. *)

  (** {1 Output} *)

  val write :
    ?wrote:(Fpath.t -> unit) -> ?over:(Fpath.t -> bool) ->
    dst:Fpath.t -> (string * bool) Fpath.map -> (unit, [> R.msg]) result
  (** [write ~log ~over ~dst m] writes the paths defined in [m] (creating
      directories if needed) relative to [dst] according to the
      content they map to and setting the executable bit according to
      the boolean. If the complete path to write already exists it is called
      with [over] to determine if it should be overwritten, defaults
      to [(fun _ -> Ok false)]. [wrote] is called with each complete path that
      was written (default to (fun _ -> ())) *)
end


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

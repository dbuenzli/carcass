(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let uchar_to_string u =
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b (Uchar.to_int u);
  Buffer.contents b

let pp_uchar ppf u = Fmt.pf ppf "%s" (uchar_to_string u)
let pp_ucharq = Fmt.quote ~mark:"'" @@ pp_uchar

let uchar = Uchar.of_int
let uchars ul = List.map uchar ul

(* Logging *)

module Log =
  (val Logs.(src_log (Src.create "carcass" ~doc:"Carcass library")) : Logs.LOG)

(* Locations *)

module Loc = struct

  (* Positions *)

  type pos = int * int

  let nil_pos = (0, -1)
  let zero_pos = (1, 0)

  (* Ranges *)

  type range = pos * pos

  let nil_range = nil_pos, nil_pos
  let zero_range = zero_pos, zero_pos

  (* Locations *)

  type src = Builtin | Cli | File of Fpath.t
  type t = src * range

  let nil = Builtin, nil_range
  let for_builtin s = Builtin, (zero_pos, (1, String.length s))
  let for_cli s = Cli, (zero_pos, (1, String.length s))
  let for_path ?(range = zero_range) p = File p, range

  let pp ppf = function
  | Builtin, loc -> Fmt.pf ppf "<builtin>:%a:" Fmt.text_loc loc
  | Cli, loc -> Fmt.pf ppf "<argv>:%a:" Fmt.text_loc loc
  | File f, loc -> Fmt.pf ppf "%a:%a:" Fpath.pp f Fmt.text_loc loc

  (* Traces *)

  type trace = t list
  let nil_trace = []
  let pp_trace =  Fmt.(vbox (list pp))
end

(* Errors *)

module Error = struct

  type unexpected = [ `Uchar of Uchar.t | `Lexeme of string | `Eoi ]

  let pp_unexpected ppf = function
  | `Uchar u -> Fmt.pf ppf "character %a (%a)" pp_ucharq u Uchar.dump u
  | `Lexeme l -> Fmt.pf ppf "lexeme %a" Fmt.(quote ~mark:"'" string) l
  | `Eoi -> Fmt.string ppf "end of input"

  type expected =
    [ `Keyword of string | `Id | `Lpar | `Rpar | `Qmark | `Comma | `Dollar
    | `Escaped_char | `Atom ]

  let pp_expected ppf = function
  | `Keyword k -> Fmt.pf ppf "%a" Fmt.(quote ~mark:"'" string) k
  | `Id -> Fmt.string ppf "identifier"
  | `Lpar -> Fmt.pf ppf "opening@ parenthesis '('"
  | `Rpar -> Fmt.pf ppf "closing@ parenthesis ')'"
  | `Qmark -> Fmt.pf ppf "closing@ quotation mark '\"'"
  | `Comma -> Fmt.string ppf "comma ','"
  | `Dollar -> Fmt.string ppf "dollar '$'"
  | `Escaped_char -> Fmt.pf ppf "escape@ character"
  | `Atom -> Fmt.string ppf "atom"

  type parse_err =
  | Illegal_bytes of string
  | Illegal_escape of Uchar.t
  | Illegal_variable_transform of string
  | Illegal_binding_id of string
  | Unclosed of [ `Quoted_atom | `Var_ref ]
  | Unexpected of unexpected * expected list

  type parse = [`Carcass_parse of parse_err * Loc.t ]

  let pp_exp_one_of ~pp_v ppf = function
  | [] -> assert false
  | [v] -> Fmt.pf ppf "expected@ %a" pp_v v
  | l -> Fmt.pf ppf "expected@ one@ of@ %a" Fmt.(list ~sep:(unit ",@ ") pp_v) l

  let pp_parse_err ppf = function
  | Illegal_bytes b ->
      Fmt.pf ppf "illegal@ byte@ sequence@ %a" String.dump b
  | Illegal_escape u ->
      Fmt.pf ppf "illegal@ escape@ character@ (%a)" Uchar.dump u
  | Illegal_variable_transform t ->
      Fmt.pf ppf "illegal@ variable@ transform@ %a" String.dump t
  | Illegal_binding_id id ->
      Fmt.pf ppf "illegal@ binding@ identifier (%a)" String.dump id
  | Unclosed `Quoted_atom ->
      Fmt.pf ppf "unclosed quoted@ atom"
  | Unclosed `Var_ref ->
      Fmt.pf ppf "unclosed variable@ reference"
  | Unexpected (u, exps) ->
      Fmt.pf ppf "unexpected@ %a@ %a"
        pp_unexpected u (pp_exp_one_of ~pp_v:pp_expected) exps

  let pp_parse ppf (`Carcass_parse (e, loc)) =
    Fmt.pf ppf "@[<v>@[syntax error, %a@]@,%a@]"
      pp_parse_err e Loc.pp loc

  exception Parse of parse
  let parse e l = raise (Parse (`Carcass_parse (e, l)))

  (* Evaluation errors *)

  type eval_id = [ `Var of string | `Bone of string | `Body of string ]

  type eval_err =
  | Circular of eval_id
  | Parse of eval_id * [ parse | R.msg ]
  | Undefined of eval_id
  | Bound_path of string * [ `Illegal | `Escapes ]

  type eval = [`Carcass_eval of eval_err * Loc.trace ]

  let pp_eval_id ppf = function
  | `Var var -> Fmt.pf ppf "variable@ '%s'" var
  | `Body id -> Fmt.pf ppf "body@ '%s'" id
  | `Bone id -> Fmt.pf ppf "bone@ '%s'" id

  let pp_eval_err ppf = function
  | Circular id ->
      Fmt.pf ppf "circular@ definition@ of@ %a" pp_eval_id id
  | Parse (id, parse) ->
      Fmt.pf ppf "definition of %a:@ " pp_eval_id id;
      begin match parse with
      | `Carcass_parse _ as parse -> pp_parse ppf parse
      | `Msg _ as msg -> R.pp_msg ppf msg
      end
  | Undefined id ->
      Fmt.pf ppf "undefined@ %a" pp_eval_id id
  | Bound_path (p, `Illegal) ->
      Fmt.pf ppf "illegal@ bound@ path@ '%s'" p
  | Bound_path (p, `Escapes) ->
      Fmt.pf ppf "bound@ path@ '%s'@ escapes@ the@ body@ root" p

  let pp_eval ppf (`Carcass_eval (e, trace)) =
    Fmt.pf ppf "@[<v>@[%a@]@,%a@]" pp_eval_err e Loc.pp_trace trace

  exception Eval of eval
  let eval e locs = raise (Eval (`Carcass_eval (e, locs)))
end

(* Carcass lexer. *)

module Lexer : sig
  type t

  val create :
    ?nln:bool -> src:Loc.src ->
    [ `String of string | `Channel of in_channel ] -> t

  val peek_pos : t -> Loc.pos
  val prev_pos : t -> Loc.pos
  val peek_loc : t -> Loc.t
  val loc : t -> Loc.pos -> Loc.pos -> Loc.t
  val peek : t -> [`Uchar of Uutf.uchar | `End ]
  val next : t -> unit

  val add : t -> Uutf.uchar -> unit
  val add_escape : t -> unit
  val lexeme : t -> string

  val skip_white : t -> unit
  val lex_while : t -> (Uutf.uchar -> bool) -> string
  val lex_uchar : t -> Uutf.uchar -> Error.expected -> unit
  val lex_id : t -> string * Loc.t
  val lex_id_or_eoi : t -> (string * Loc.t) option
  val lex_keyword : Error.expected list -> t -> (string * Loc.t) option

  val is_atom_end : Uutf.uchar -> bool
  val is_quoted_atom_end : Uutf.uchar -> bool
  val lex_atom : t -> string * Loc.t
end = struct

  type t =
    { src : Loc.src;
      d : Uutf.decoder;
      buf : Buffer.t;
      mutable prev_line : int;
      mutable prev_col : int;
      mutable peek : [ `Uchar of Uutf.uchar | `End | `Start ]; }

  let create ?(nln = true) ~src input =
    let nln = if nln then Some (`ASCII 0x000A (* LF *)) else None in
    let d = Uutf.decoder ~encoding:`UTF_8 ?nln input in
    { src; d; buf = Buffer.create 1024;
      prev_line = 0; prev_col = 0;
      peek = `Start }

  let peek_pos l = Uutf.decoder_line l.d, Uutf.decoder_col l.d
  let peek_loc l = let loc = peek_pos l in l.src, (loc, loc)
  let prev_pos l = l.prev_line, l.prev_col
  let loc l start stop = l.src, (start, stop)

  let next l =
    l.prev_line <- Uutf.decoder_line l.d;
    l.prev_col <- Uutf.decoder_col l.d;
    match Uutf.decode l.d with
    | (`Uchar _ | `End) as c -> l.peek <- c
    | `Malformed b -> Error.(parse (Illegal_bytes b) (peek_loc l))
    | `Await -> assert false

  let rec peek l = match l.peek with
  | `Start -> next l; peek l
  |  (`Uchar _ | `End) as d -> d

  let add l u = Uutf.Buffer.add_utf_8 l.buf u
  let add_escape l = match peek l with
  | `Uchar (0x0020 (* space *) | 0x0022 (* quote *) | 0x005C (* \ *) as u) ->
      next l; add l u
  | `Uchar 0x006E (* n *)  ->
      next l; add l 0x000A (* LF *)
  | `Uchar 0x000A (* LF *) ->
      let rec loop l = match peek l with              (* skip initial white *)
      | `Uchar (0x0020 (* space *) | 0x0009 (* tab *)) -> next l; loop l
      |  _ -> ()
      in
      next l; loop l
  | `Uchar u ->
      Error.(parse (Illegal_escape (uchar u)) (peek_loc l))
  | `End ->
      Error.(parse (Unexpected (`Eoi, [`Escaped_char])) (peek_loc l))

  let lexeme l =
    let s = Buffer.contents l.buf in Buffer.clear l.buf; s

  let is_white = function
  | 0x0020 (* sp *) | 0x0009 (* tab *) | 0x000A (* LF *)
  | 0x000B (* vt *) | 0x000C (* FF *) -> true
  | _ -> false

  let rec skip_white l = match peek l with
  | `Uchar u when is_white u ->
      let rec loop l = match peek l with
      | `Uchar u when is_white u -> next l; loop l
      | _ -> ()
      in
      next l; loop l; skip_white l
  | `Uchar 0x0023 (* #, comment *) ->
      let rec loop l = match peek l with
      | `Uchar 0x000A (* LF *) -> ()
      | _ -> next l; loop l
      in
      next l; loop l; next l; skip_white l
  | `End | `Uchar _ -> ()

  let rec lex_while l sat = match peek l with
  | `Uchar u when sat u -> add l u; next l; lex_while l sat
  | _ -> lexeme l

  let is_id_char u =
    (0x0030 <= u && u <= 0x0039) || (* 0 .. 9 *)
    (0x0041 <= u && u <= 0x005A) || (* A .. Z *)
    (0x0061 <= u && u <= 0x007A) || (* a .. z *)
    (0x005F = u)                    (* _ *)

  let lex_uchar l u exp = match (skip_white l; peek l) with
  | `Uchar u' when u' = u -> ()
  | `Uchar u' ->
      Error.(parse (Unexpected (`Uchar (uchar u), [exp])) (peek_loc l))
  | `End ->
      Error.(parse (Unexpected (`Eoi, [exp])) (peek_loc l))

  let lex_id_or_eoi l = match (skip_white l; peek l) with
  | `Uchar u when is_id_char u ->
      let start = peek_pos l in
      let id = lex_while l is_id_char in
      Some (id, loc l start (prev_pos l))
  | `Uchar u ->
      Error.(parse (Unexpected (`Uchar (uchar u), [`Id])) (peek_loc l))
  | `End -> None

  let lex_id l = match lex_id_or_eoi l with
  | None -> Error.(parse (Unexpected (`Eoi, [`Id])) (peek_loc l))
  | Some id -> id

  let is_keyword_char u = (0x0061 <= u && u <= 0x007A) (* a .. z *)
  let lex_keyword ks l = match (skip_white l; peek l) with
  | `Uchar u when is_keyword_char u ->
      let start = peek_pos l in
      let keyword = lex_while l is_keyword_char in
      Some (keyword, loc l start (prev_pos l))
  | `Uchar u -> Error.(parse (Unexpected (`Uchar (uchar u), ks)) (peek_loc l))
  | `End -> None

  (* Lexing atoms *)

  let is_quoted_atom_end = function 0x0022 (* quote *) -> true | _ -> false
  let is_atom_end = function
  | u when is_white u -> true
  | 0x0022 (* qmark *) | 0x0023 (* # *) -> true
  | _ -> false

  let lex_quoted_atom l =
    let rec loop l start = match peek l with
    | `Uchar 0x005C (* \ *) ->
        next l; add_escape l; loop l start
    | `Uchar 0x0022 (* qmark *) ->
        next l; lexeme l, loc l start (prev_pos l)
    | `Uchar u ->
        add l u; next l; loop l start
    | `End ->
        Error.(parse (Unclosed `Quoted_atom) (loc l start (peek_pos l)))
    in
    let start = peek_pos l in
    next l; loop l start

  let lex_simple_atom l =
    let rec loop l start = match peek l with
    | `Uchar u when not (is_atom_end u) -> add l u; next l; loop l start
    | _ -> lexeme l, loc l start (prev_pos l)
    in
    loop l (peek_pos l)

  let lex_atom l = match (skip_white l; peek l) with
  | `Uchar 0x0022 (* qmark *) -> lex_quoted_atom l
  | `Uchar u -> lex_simple_atom l
  | `End -> Error.(parse (Unexpected (`Eoi, [`Atom])) (peek_loc l))
end

(* Patterns *)

module Pat = struct

  (* Variable reference transforms *)

  type transform =
    | Uppercase | Lowercase | Capitalize | Uncapitalize | Indent of string

  let transform t s = match t with
  | None -> s
  | Some Uppercase -> String.Ascii.uppercase s
  | Some Lowercase -> String.Ascii.lowercase s
  | Some Capitalize -> String.Ascii.capitalize s
  | Some Uncapitalize -> String.Ascii.uncapitalize s
  | Some Indent prefix ->
      let lines = String.cuts ~sep:"\n" s in
      let add_prefix l =
        let p = prefix ^ l in
        if String.for_all Char.Ascii.is_white p then "" else p
      in
      String.concat ~sep:"\n" (List.rev @@ List.rev_map add_prefix lines)

  let transform_to_string = function
  | Uppercase -> "uppercase"
  | Lowercase -> "lowercase"
  | Capitalize -> "capitalize"
  | Uncapitalize -> "uncapitalize"
  | Indent prefix -> strf "indent(%a)" String.dump prefix

  let pp_transform = Fmt.of_to_string transform_to_string

  (* Patterns *)

  type lexeme = Lit of string | Var of string * transform option
  type t = (lexeme * Loc.t) list * Loc.t

  (* Parsing *)

  let parse_transform_arg l =
    Lexer.lex_uchar l 0x0028 `Lpar;
    let atom, _ = Lexer.(next l; skip_white l; lex_atom l) in
    Lexer.lex_uchar l 0x0029 `Rpar;
    Lexer.next l;
    atom

  let parse_transform l =
    let tr, loc = Lexer.lex_id l in
    match tr with
    | "uppercase" -> Uppercase
    | "lowercase" -> Lowercase
    | "capitalize" -> Capitalize
    | "uncapitalize" -> Uncapitalize
    | "indent" -> Indent (parse_transform_arg l)
    | _ -> Error.(parse (Illegal_variable_transform tr) loc)

  let parse_variable_reference l start = (* $( already eaten *)
    let id = String.Ascii.uppercase @@ fst (Lexer.lex_id l) in
    match (Lexer.skip_white l; Lexer.peek l) with
    | `Uchar 0x0029 (* ) *) ->
        Lexer.next l;
        Var (id, None), Lexer.(loc l start (prev_pos l))
    | `Uchar 0x002C (* , *) ->
        let tr = (Lexer.next l; parse_transform l) in
        begin match Lexer.peek l with
        | `Uchar 0x0029 (* ) *) ->
            Lexer.next l;
            Var (id, Some tr), Lexer.(loc l start (prev_pos l))
        | `Uchar u ->
            Error.(parse (Unexpected (`Uchar (uchar u), [`Rpar]))
                     (Lexer.peek_loc l))
        | `End ->
            Error.(parse (Unclosed (`Var_ref)) (Lexer.peek_loc l))
        end
    | `Uchar u ->
        Error.(parse (Unexpected (`Uchar (uchar u), [`Rpar; `Comma]))
                 (Lexer.peek_loc l))
    | `End ->
            Error.(parse (Unclosed `Var_ref) (Lexer.peek_loc l))

  let parse_pat l ~escapes stop =
    let rec loop l start acc = match Lexer.peek l with
    | `Uchar 0x0024 (* $ *) ->
        let vref_start = Lexer.peek_pos l in
        begin match (Lexer.next l; Lexer.peek l) with
        | `Uchar 0x0024 (* $ *) -> (* $$ escape *)
            Lexer.add l 0x0024; Lexer.next l; loop l start acc
        | `Uchar 0x0028 (* ( *) ->
            let acc = match Lexer.lexeme l with
            | "" (* no running litteral *) -> acc
            | lit -> (Lit lit, Lexer.loc l start vref_start) :: acc
            in
            let vref = (Lexer.next l; parse_variable_reference l vref_start) in
            loop l (Lexer.peek_pos l) (vref :: acc)
        | `Uchar u ->
            Error.(parse (Unexpected (`Uchar (uchar u), [`Lpar; `Dollar]))
                     (Lexer.peek_loc l))
        | `End ->
            Error.(parse (Unclosed `Var_ref) (Lexer.peek_loc l))
        end
    | `Uchar 0x005C (* \ *) when escapes ->
        Lexer.next l; Lexer.add_escape l; loop l start acc
    | `Uchar u when not (stop u) ->
        Lexer.add l u; Lexer.next l; loop l start acc
    | `Uchar _  | `End ->
        let lit = Lexer.(Lit (lexeme l), loc l start (prev_pos l)) in
        List.rev (lit :: acc)
    in
    loop l (Lexer.peek_pos l) []

  let parse_or_eoi l = match (Lexer.skip_white l; Lexer.peek l) with
  | `Uchar 0x0022 (* qmark *) ->
      let start = Lexer.peek_pos l in
      let pat = (Lexer.next l;
                 parse_pat l ~escapes:true Lexer.is_quoted_atom_end)
      in
      begin match Lexer.peek l with
      | `Uchar 0x0022 (* qmark *) ->
          Lexer.next l; Some (pat, Lexer.(loc l start (prev_pos l)))
      | `Uchar u ->
          Error.(parse (Unexpected (`Uchar (uchar u), [`Qmark]))
                   (Lexer.peek_loc l))
      | `End ->
          Error.(parse (Unclosed `Quoted_atom)
                   Lexer.(loc l start (peek_pos l)))
      end
  | `Uchar u ->
      let start = Lexer.peek_pos l in
      let pat = parse_pat l ~escapes:false Lexer.is_atom_end in
      Some (pat, Lexer.(loc l start (prev_pos l)))
  | `End -> None

  let parse l = match parse_or_eoi l with
  | None -> Error.(parse (Unexpected (`Eoi, [`Atom])) (Lexer.peek_loc l))
  | Some pat -> pat

  let empty = [], Loc.for_builtin ""

  let dom (p, _) =
    let rec loop acc = function
    | (Lit _, _) :: ls -> loop acc ls
    | (Var (id, _), _) :: ls -> loop (String.Set.add id acc) ls
    | [] -> acc
    in
    loop String.Set.empty p

  let equal p p' = p = p'
  let compare p p' = Pervasives.compare p p'
  let to_string ?(flesh = false) (p, _) =
    let b = Buffer.create 255 in
    let add = function
    | (Lit l, _) ->
        let max_i = String.length l - 1 in
        let rec loop start i =
          if i > max_i then Buffer.add_substring b l start (i - start) else
          let next = i + 1 in
          match l.[i] with
          | '$' -> (* escape $ *)
              Buffer.add_substring b l start (next - start);
              Buffer.add_char b '$';
              loop next next
          | ('"' | '\\' as c) when flesh -> (* escape '"' and '\\' *)
              Buffer.add_substring b l start (next - start - 1);
              Buffer.add_char b '\\';
              Buffer.add_char b c;
              loop next next
          | _ -> loop start next
        in
        loop 0 0
    | (Var (v, tr), _) ->
        Buffer.add_string b "$(";
        Buffer.add_string b v;
        begin match tr with
        | None -> ()
        | Some tr ->
            Buffer.add_char b ',';
            Buffer.add_string b (transform_to_string tr)
        end;
        Buffer.add_string b ")";
    in
    List.iter add p;
    Buffer.contents b

  let of_input ?(flesh = false) ~src i =
    try
      let l = Lexer.create ~nln:false ~src i in
      let start = Lexer.peek_pos l in
      let pat = parse_pat l ~escapes:flesh (fun _ -> false) in
      let stop = Lexer.peek_pos l in
      let loc = src, (start, stop) in
      Ok (pat, loc)
    with Error.Parse (`Carcass_parse _ as e) -> Error e

  let pp ?flesh = Fmt.of_to_string (to_string ?flesh)

  (* Substitution *)

  let subst subst (p, loc) =
    let rec loop acc = function
    | (Lit _, _ as lit) :: p -> loop (lit :: acc) p
    | (Var (v, tr), loc as var) :: p ->
        begin match subst v with
        | None -> loop (var :: acc) p
        | Some lit -> loop ((Lit (transform tr lit), loc) :: acc) p
        end
    | [] -> List.rev acc
    in
    loop [] p, loc

  (* Evaluation *)

  type env =
    { defs : t String.map;
      undef : string -> (t, Error.parse) result option;
      mutable eval_cache : string String.map; }

  let env ?(undef = fun _ -> None) defs =
    { defs; undef; eval_cache = String.Map.empty }

  let eval_cache env var = String.Map.find var env.eval_cache
  let var_def locs env var = match String.Map.find var env.defs with
  | Some pat -> pat
  | None ->
      if String.is_prefix "CARCASS_MATCH_" var then empty else
      match env.undef var with
      | Some (Ok pat) -> pat
      | None -> Error.(eval (Undefined (`Var var)) locs)
      | Some Error (`Carcass_parse _ as e) ->
          Error.(eval (Parse (`Var var, e)) locs)
  let rec eval env stack b seen locs = function
  | [] ->
      begin match stack with
      | [] -> Buffer.contents b
      | (b', seen, locs, pat, var) :: stack ->
          let value = Buffer.contents b in
          env.eval_cache <- String.Map.add var value env.eval_cache;
          eval env stack b' seen locs pat
      end
  | (Lit lit, _) :: p ->
      Buffer.add_string b lit; eval env stack b seen locs p
  | (Var (var, tr), ref_loc) :: p as pat ->
      if String.Set.mem var seen
      then Error.(eval (Circular (`Var var)) (ref_loc :: locs)) else
      match eval_cache env var with
      | Some value ->
          Buffer.add_string b (transform tr value);
          eval env stack b seen locs p
      | None ->
          let stack = (b, seen, locs, pat, var) :: stack in
          let pat, def_loc = var_def (ref_loc :: locs) env var in
          let b = Buffer.create 255 in
          let locs = def_loc :: ref_loc :: locs in
          let seen = String.Set.add var seen in
          eval env stack b seen locs pat

  let env_var_value env var =
    match String.Map.find var env.defs with
    | None -> None
    | Some (_, loc) ->
        let pat = [Var (var, None), Loc.nil] in
        let result =
          try Ok (eval env [] (Buffer.create 255) String.Set.empty [] pat, loc)
          with Error.Eval (`Carcass_eval (e, trace)) ->
            let trace = List.(rev (tl (rev trace))) in
            (Result.Error (`Carcass_eval (e, trace)))
        in
        Some result

  let eval env (pat, loc) =
    try Ok (eval env [] (Buffer.create 255) String.Set.empty [loc] pat, loc)
    with Error.Eval (`Carcass_eval _ as e) -> Error e

  (* Matching *)

  let match_lit ~lit start s = (* matches [lit] at [start] in [s]. *)
    let l_max = String.length lit - 1 in
    let s_max = String.length s - start - 1 in
    if l_max > s_max then None else
    let rec loop i =
      if i > l_max then Some (start + l_max + 1) else
      if lit.[i] <> s.[start + i] then None else
      loop (i + 1)
    in
    loop 0

  let query ?(init = String.Map.empty) (p, _) s =
    (* Not tail-recursive but bounded by number of variables *)
    let rec loop env start s = function
    | [] -> if start = String.length s then Some env else None
    | (Lit lit, _) :: p ->
        begin match (match_lit ~lit start s) with
        | None -> None
        | Some start -> loop env start s p
        end
    | (Var (v, _), _) :: p ->
        let rec try_match next_start =
          if next_start < start then None else
          match loop env next_start s p with
          | None -> try_match (next_start - 1)
          | Some env' ->
              let value =
                String.with_index_range s ~first:start ~last:(next_start - 1)
              in
              Some (String.Map.add v value env')
        in
        try_match (String.length s) (* Longest match first. *)
    in
    loop init 0 s p
end

(* Environments *)

module Env = struct

  (* Directories *)

  let etc_dir = Carcass_etc.dir
  let user_dir () = OS.Dir.user () >>| fun home -> Fpath.(home / ".carcass")

  (* Environments *)

  type t =
    { dirs : Fpath.t list;
      flesh : Fpath.t list;
      cli : (string * Pat.t) list; }

  let v ~no_user_dir ~no_dot_dirs ~dirs ~flesh ~cli =
    let dirs =
      if no_dot_dirs then dirs else
      let rec loop dir dirs =
        let carcass = Fpath.(dir / ".carcass") in
        let dirs =
          begin OS.Dir.exists carcass >>| function
          | true -> carcass :: dirs
          | false -> dirs
          end
          |> Log.on_error_msg ~level:Logs.Warning ~use:(fun () -> dirs)
        in
        let dir = Fpath.parent dir in
        if Fpath.is_root dir then dirs else loop dir dirs
      in
      (OS.Dir.current () >>| fun dir -> loop dir dirs)
      |> Log.on_error_msg ~level:Logs.Warning ~use:(fun () -> dirs)
    in
    let dirs =
      if no_user_dir then dirs else
      begin user_dir () >>| fun u ->
        if List.exists (Fpath.equal u) dirs then dirs else (u :: dirs)
      end
      |> R.reword_error_msg ~replace:true
        (fun err -> R.msgf "No user ~/.carcass directory: %s" err)
      |> Log.on_error_msg ~level:Logs.Warning ~use:(fun () -> dirs)
    in
    { dirs; flesh; cli }

  let flesh_files env =
    let add_dir_flesh acc dir =
      let flesh = Fpath.(dir / "flesh") in
      begin OS.File.exists flesh >>| function
      | true -> flesh :: acc
      | false -> acc
      end
      |> Log.on_error_msg ~level:Logs.Warning ~use:(fun () -> acc)
    in
    List.(rev_append (fold_left add_dir_flesh [] env.dirs) env.flesh)

  let path_exists p =
    OS.Path.exists p
    |> Log.on_error_msg ~level:Logs.Warning ~use:(fun () -> false)

  let file_with_id ?ext env id =
    if Fpath.(filename id = "flesh") then None else
    if Fpath.(equal id OS.File.dash) then Some id else
    if Fpath.(is_current_dir ~prefix:true id || is_abs id) then
      (if path_exists id then Some id else None)
    else
    let id = match ext with
    | None -> id
    | Some e -> if Fpath.has_ext e id then id else Fpath.(id + e)
    in
    let rec loop = function
    | [] -> None
    | d :: dirs ->
        let p = Fpath.(d // id) in
        if path_exists p
        then (Log.info (fun m -> m "Hit  %a" Fpath.pp p); Some p)
        else (Log.info (fun m -> m "Miss %a" Fpath.pp p); loop dirs)
    in
    loop (List.rev env.dirs)

  let list_ids ?(hidden = false) ~is_kind env =
    let is_id p =
      OS.File.exists p >>| function
      | false -> false
      | true ->
          let fname = Fpath.filename p in
          let hidden_id = String.is_prefix "_" fname in
          let flesh_file = fname = "flesh" in
          let is_kind = is_kind p in
          not flesh_file && is_kind && (not hidden_id || hidden)
    in
    let rec loop acc = function
    | [] -> acc
    | d :: dirs ->
        let add_bone p acc =
          let id = match Fpath.rem_prefix d p with
          | None -> assert false
          | Some id -> id
          in
          if Fpath.Map.mem id acc then acc else Fpath.Map.add id p acc
        in
        let acc =
          if not (path_exists d) then acc else
          (OS.Dir.fold_contents ~elements:(`Sat is_id) add_bone acc d)
          |> Log.on_error_msg ~level:Logs.Warning ~use:(fun () -> acc)
        in
        loop acc dirs
    in
    loop Fpath.Map.empty (List.rev env.dirs)
end

(* Ask values *)

module Ask = struct

  type ('a, 'b) t = ('a, Format.formatter, unit, 'b) Pervasives.format4 -> 'a

  let _value ?(ppf = Fmt.stdout) ~parse fmt =
    let k ppf = try parse (input_line stdin) with
    | End_of_file -> parse ""
    in
    Format.kfprintf k ppf ("@[<1>" ^^ fmt ^^ "@]@?")

  let value = _value

  let pattern ?ppf fmt =
    let parse s =
      Pat.of_input ~flesh:true ~src:(Loc.File OS.File.dash) (`String s)
    in
    _value ?ppf ~parse fmt

  let bool ?ppf ~default fmt =
    let choices = format_of_string @@ match default with
    | true -> " [Y/n] "
    | false -> " [y/N] "
    in
    let parse s = match String.Ascii.lowercase s with
    | "yes" | "y" | "1" -> true
    | "no" | "n" | "0" -> false
    | _ -> default
    in
    _value ?ppf ~parse (fmt ^^ choices)

  let string ?ppf ~default fmt =
    let parse = function "" -> default | s -> s in
    _value ?ppf ~parse fmt
end

(* Flesh (variable definitions) *)

module Flesh = struct

  (* Builtins *)

  let carcass_year () =
    let current_year = strf "%d" @@ Unix.((gmtime @@ time ()).tm_year) + 1900 in
    let loc = Loc.for_builtin current_year in
    [Pat.Lit current_year, loc], loc

  let builtins =
    String.Map.(empty |> add "CARCASS_YEAR" (carcass_year ()))

  (* Flesh *)

  type t = Pat.t String.map

  let _of_input ~src acc i =
    let l = Lexer.create ~src i in
    let rec loop l acc = match Lexer.lex_id_or_eoi l with
    | None -> Ok acc
    | Some (id, (src, (def_start, _))) ->
        let id = String.Ascii.uppercase id in
        let (pat, (_, (_, def_end))) = Pat.parse l in
        let loc = src, (def_start, def_end) in
        loop l (String.Map.add id (pat, loc) acc)
    in
    try loop l acc with Error.Parse (`Carcass_parse _ as e) -> Error e

  let of_input ?(init = builtins) ~src i = _of_input ~src builtins i

  let add_flesh_file acc f =
    let read ic acc = _of_input ~src:(Loc.File f) acc (`Channel ic) in
    OS.File.with_ic f read acc
    |> Log.on_error_msg ~level:Logs.Warning ~use:(fun () -> Ok acc)

  let rec add_flesh_files acc = function
  | [] -> Ok acc
  | f :: fs ->
      Log.info (fun m -> m "Reading flesh %a" Fpath.pp f);
      add_flesh_file acc f >>= fun acc -> add_flesh_files acc fs

  let rec add_cli_flesh acc defs =
    let add acc (id, pat) =
      String.Map.add (String.Ascii.uppercase id) pat acc
    in
    List.fold_left add acc defs

  let of_env ?(init = builtins) env =
    Ok init
    >>= fun defs -> add_flesh_files defs (Env.flesh_files env)
    >>= fun defs -> Ok (add_cli_flesh defs env.Env.cli)

  let pp_def = Fmt.quote (Pat.pp ~flesh:true)
  let pp =
    let pp_binding ppf (var, def) = Fmt.pf ppf "%s %a" var pp_def def in
    Fmt.vbox (String.Map.pp pp_binding)
end

(* Bones *)

module Bone = struct

  (* Lookup *)

  type id = Fpath.t

  let find env id =
    if Fpath.(has_ext ".body" id) then None else Env.file_with_id env id

  let list ?hidden env =
    let is_bone p = not Fpath.(has_ext ".body" p) in
    Env.list_ids ?hidden ~is_kind:is_bone env

  (* Bones *)

  type content = Binary of string | Pat of Pat.t
  type t = { id : id; content : content; is_exec : bool }

  let id b = b.id
  let content b = b.content
  let is_exec b = b.is_exec

  let of_input ?(trim = false) ~src (`String bytes) ~is_exec id =
    match String.exists (Char.equal '\x00') bytes with
    | true -> Ok { id; content = Binary bytes; is_exec }
    | false ->
        let bytes = if trim then String.trim bytes else bytes in
        Pat.of_input ~flesh:false ~src (`String bytes)
        >>= fun pat -> Ok { id; content = Pat pat; is_exec }

  let of_path ?trim p id =
    OS.Path.Mode.get p
    >>= fun mode -> OS.File.read p
    >>= fun bytes ->
    let is_exec = (mode land 0o100) <> 0 in
    of_input ?trim ~src:(Loc.File p) (`String bytes) ~is_exec id

  let eval env b = match b.content with
  | Binary b -> Ok b
  | Pat p ->
      match Pat.eval env p with
      | Error _ as e -> e
      | Ok (s, _) -> Ok s
end

module Body = struct

  (* Lookup *)

  type id = Fpath.t

  let find env id = Env.file_with_id ~ext:".body" env id
  let list ?hidden env =
    let is_body p = Fpath.(has_ext ".body" p) in
    Env.list_ids ?hidden ~is_kind:is_body env

  (* Bodies *)

  type binding_id = Fpath.t
  type t =
    { id : Fpath.t;
      doc : string * string;
      var_docs : string String.map;
      bindings : (Pat.t * (binding_id * Loc.t)) list; }

  let id b = b.id
  let doc b = b.doc
  let var_docs b = b.var_docs
  let bindings b = b.bindings

  let parse_doc l =
    let exp = [`Keyword "doc" ] in
    match Lexer.lex_keyword exp l with
    | None ->
        Error.(parse (Unexpected (`Eoi, exp)) (Lexer.peek_loc l))
    | Some (k, loc) when k <> "doc" ->
        Error.(parse (Unexpected (`Lexeme k, exp)) loc)
    | Some _ ->
        let synopsis, _ = Lexer.lex_atom l in
        let descr, _ = Lexer.lex_atom l in
        synopsis, descr

  let rec parse_var_docs l =
    let exp = [`Keyword "var"; `Keyword "bind"] in
    let rec loop acc = match Lexer.lex_keyword exp l with
    | None ->
        acc, None
    | Some (k, loc) as lexeme when k <> "var" ->
        if k <> "bind"
        then Error.(parse (Unexpected (`Lexeme k, exp)) loc)
        else acc, lexeme
    | Some _ ->
        let var = String.Ascii.uppercase @@ fst (Lexer.lex_id l) in
        let doc = fst (Lexer.lex_atom l) in
        loop (String.Map.add var doc acc)
    in
    loop String.Map.empty

  let parse_bindings l peek =
    let exp = [ `Keyword "bind" ] in
    let rec loop acc = function
    | None ->
        List.rev acc
    | Some (k, loc) when k <> "bind" ->
        Error.(parse (Unexpected (`Lexeme k, exp)) loc)
    | Some _ ->
        let pat = Pat.parse l in
        let atom, loc = Lexer.lex_atom l in
        match Fpath.of_string atom with
        | Error _ -> Error.(parse (Illegal_binding_id atom) loc)
        | Ok id ->
            if Fpath.(is_current_dir ~prefix:true id || is_abs id)
            then Error.(parse (Illegal_binding_id atom) loc)
            else loop ((pat, (id, loc)) :: acc) (Lexer.lex_keyword exp l)
    in
    loop [] peek

  let of_input ~src i id =
    let id = if Fpath.has_ext ".body" id then id else Fpath.(id + ".body") in
    let l = Lexer.create ~src i in
    try
      let doc = parse_doc l in
      let var_docs, peek = parse_var_docs l in
      let bindings = parse_bindings l peek in
      Ok { id; doc; var_docs; bindings }
    with Error.Parse (`Carcass_parse _ as e) -> Error e

  let of_path p id =
    OS.File.read p >>= fun bytes ->
    of_input ~src:(Loc.File p) (`String bytes) id

  (* Evaluation *)

  let eval_paths env penv b =
    let rec loop stack acc seen locs root = function
    | [] ->
        begin match stack with
        | [] -> Ok acc
        | (seen, locs, root, binds) :: stack ->
            loop stack acc seen locs root binds
        end
    | (ppat, (id, loc)) :: binds ->
        let path = match Pat.eval penv ppat with
        | Error (`Carcass_eval (e, t)) ->
            Error.(eval e (List.(rev (rev_append locs (rev t)))))
        | Ok (p, ploc) ->
            match Fpath.of_string p with
            | Error _ -> Error.(eval (Bound_path (p, `Illegal)) (ploc :: locs))
            | Ok p ->
                let p' = Fpath.(root // p) in
                if (Fpath.is_rooted ~root p') then p' else
                let p' = Fpath.to_string p' in
                Error.(eval (Bound_path (p', `Escapes)) (ploc :: locs))
        in
        if not (Fpath.has_ext ".body" id) then begin
          let path = Fpath.normalize path in
          let acc = Fpath.Map.add path (id, loc :: locs) acc in
          loop stack acc seen locs root binds
        end else begin
          let locs = loc :: locs in
          if Fpath.Set.mem id seen
          then Error.(eval (Circular (`Body (Fpath.to_string id))) locs) else
          match find env id with
          | None ->
              Error.(eval (Undefined (`Body (Fpath.to_string id))) locs)
          | Some p ->
              match of_path p id with
              | Error err ->
                  Error.(eval (Parse ((`Body (Fpath.to_string id)), err)) locs)
              | Ok b ->
                  let stack = (seen, List.tl locs, root, binds) :: stack in
                  let seen = Fpath.Set.add id seen in
                  loop stack acc seen locs path b.bindings
        end
    in
    try
      let seen = Fpath.Set.singleton b.id in
      loop [] Fpath.Map.empty seen [] (Fpath.v ".") b.bindings
    with Error.Eval (`Carcass_eval _ as e) -> Error e

  let eval_bones env penv m =
    let eval bpath (id, locs) acc = match Bone.find env id with
    | None -> Error.(eval (Undefined (`Bone (Fpath.to_string id))) locs)
    | Some p ->
        match Bone.of_path p id with
        | Error e -> Error.(eval (Parse (`Bone (Fpath.to_string id), e)) locs)
        | Ok b ->
            match Bone.eval penv b with
            | Error (`Carcass_eval (e, t)) ->
                Error.(eval e (List.(rev_append (rev locs) t)))
            | Ok contents ->
                Fpath.Map.add bpath (contents, Bone.is_exec b) acc
    in
    try Ok (Fpath.Map.fold eval m Fpath.Map.empty)
    with Error.Eval (`Carcass_eval _ as e) -> Error e

  let write ?(wrote = fun _ -> ()) ?(over = fun _ -> false) ~dst m =
    let do_write p = OS.File.exists p >>| fun exists -> not exists || over p in
    let write_path p (c, exec) acc = match acc with
    | Error _ as e -> e
    | Ok () ->
        let p = Fpath.(dst // p) in
        let dir = Fpath.parent p in
        let mode = if exec then 0o733 else 0o622 in
        do_write p >>= function
        | false -> Ok ()
        | true ->
            OS.Dir.create ~path:true dir
            >>= fun _ -> OS.File.write ~mode p c
            >>| fun () -> wrote p
    in
    Fpath.Map.fold write_path m (Ok ())
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

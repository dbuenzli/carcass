(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let carcass_manual = "Carcass manual"
let version = "%%VERSION%%"

(* Help manuals *)

let basics =
  ("CARCASS-BASICS", 7, "", version, carcass_manual),
  [ `S "NAME";
    `P "carcass-basics - short introduction to carcass";
    `S "DESCRIPTION";
    `P "carcass helps you to create software project boilerplate. This can
        range from  a single source file with your copyright and licensing
        information to whole project scaffoldings.";
    `S "SETUP";
    `P "The first thing you should do after having installed carcass is
        to invoke:";
    `Pre "> carcass setup";
    `P "This will ask you a few questions to setup your personal information
        in the ~/.carcass/flesh file and copy a few default bones and bodies
        to ~/.carcass.";
    `P "Having done this take the time to further adjust your personal
        variables in ~/.carcass/flesh according to your wishes.";
    `S "CONCEPTS";
    `P "There are three kinds of files in carcass:";
    `I ("$(i,flesh) files", "These files hold sequences of variable definitions.
        See for example the ~/.carcass/flesh file.");
    `I ("$(i,bone) files", "Bone files allow to create a single file. They
        are arbitrary files located in a carcass directory. Files which
        are detected as text files (i.e. without a null byte) will be parsed
        as UTF-8 encoded file and have variable references of the
        form $$$$(VAR) substituted with the definitions found in flesh
        files.");
    `I ("$(i,body) files", "Body files allow to create whole file hierarchies
         out of bones and flesh. They are the files located in a carcass
         directory that end with `.body`.");
    `P "To each kind of file there is a corresponding carcass command. Let's
        review them in turn.";
    `S "FLESH";
    `P "The $(b,flesh) command looks up a flesh variable by its identifier,
        evaluates its definition and writes the result on stdout. For example:";
    `Pre "> carcass flesh contact";
    `P "To see the raw, unevaluated, definition of a variable, use the
        $(b,-r) option.";
    `Pre "> carcass flesh -r copyright_year";
    `P "The $(b,-p) option outputs all variables, in flesh file syntax, that
        are prefixed by the given identifier. The $(b,-l) option prepends the
        output with the location of the definition. Using this, the following
        invocation:";
    `Pre "> carcass flesh -r -l -p \"\"";
    `P  "Lists all variable definitions with their location; the empty string
         is the prefix of any any string. See carcass-flesh(1) for
         more information about the $(b,flesh) command.";
    `S "BONE";
    `P "The $(b,bone) command looks up a bone by its identifier,
        evaluates its variable references according to the flesh in
        the environment and writes the result on stdout.";
    `Pre "> carcass bone ocaml/src";
    `P "If a variable definition can't be found, it will ask for it
        interactively. It is also possible to define or
        override variables on the command line:";
    `Pre "> carcass bone ocaml/src copyright_year 2008";
    `P "The bone command also supports the $(b,-r) and $(b,-l) options
        to access the raw definition and output, on stderr,
        the location of the bone.";
    `Pre "> carcass bone -r -l ocaml/src";
    `P "To get the list of available bones use:";
    `Pre "> carcass info bones";
    `P "See carcass-bone(1) for more information about the $(b,bone) command.
        Regarding bones  you may also find the $(b,match) command useful but
        I let you read about it in carcass-match(1).";
    `S "BODY";
    `P "The $(b,body) command looks up a body by its identifier. This in turn
        looks up the bones and bodies it is made of, evaluates their variable
        references according to the flesh in the environment and creates the
        file hierarchy it defines in a given destination directory. For example
        the following creates an OCaml module (mli/ml files) in the /tmp
        directory:";
    `Pre "> carcass body ocaml/mod /tmp";
    `P "Here again it is possible to specify flesh variables on the command
        line.";
    `Pre "> carcass body ocaml/mod /tmp name m";
    `P "Carcass never overwrites files without confirming, unless the
        option $(b,--force) is used. The raw definition and location of the
        bone can be consulted with the usual options:";
    `Pre "> carcass body -r -l ocaml/mod";
    `P "To get the list of available bodies issue:";
    `Pre "> carcass info bodies";
    `P "And if you'd like more information about their variables and purpose
        use:";
    `Pre "> carcass info bodies -d";
    `P "See carcass-body(1) for more information about the $(b,body) command.";
    `S "LOOKUP PROCEDURES";
    `P "Flesh, bones and bodies definitions are looked up according to
        procedures that are defined precisely in carcass-lookup(5).";
    `S "ADDING NEW BONES AND BODIES";
    `P "To add new bones and bodies simply create files in your ~/.carcass
        directory. The examples there with the help of the formal
        definitions of carcass-syntax(5) should be sufficient to get
        you started.";
    `P "One note about the bones and bodies that you find in ~/.carcass
        that start with an '_'. These can be consulted like any other body
        or bone, however they don't get listed by 'carcass info'; unless the
        $(b,--hidden) option is used.";
    `S "TROUBLESHOOTING";
    `P "If the output doesn't quite correspond to what you expect, remember
        that most commands have the $(b,-r) and $(b,-l) options to output
        raw definitions and their location. Invoking the tool with $(b,-v) may
        also help in figuring out where the bones and bodies are picked
        up from.";
    `S "SEE ALSO";
    `P "carcass(1), carcass-syntax(5), carcass-lookup(5)" ]

let lookup =
  ("CARCASS-LOOKUP", 5, "", version, carcass_manual),
  [ `S "NAME";
    `P "carcass-lookup - carcass lookup procedures";
    `S "FLESH VARIABLE LOOKUP";
    `P "Most commands allow to define variables: on the command line as
        positional arguments, in flesh files specified via the
        $(b,--flesh) option and in carcass directories specified via
        the $(b,--carcass) option.";
    `P "In a flesh file the last (re)definition takes over.";
    `P "For a given variable identifier the first definition found in
        the following order takes over the others:";
    `I ("1. Positional command line arguments", "Starting from the rightmost
        $(i,ID) $(i,DEF) pair.");
    `I ("2. Command line flesh files", "Starting from the rightmost one, any
         file specified with the $(b,--flesh) option.");
    `I ("3. Command line carcass directory flesh files",
         "Starting from the rightmost one,
         any file $(i,DIR)/flesh with $(i,DIR) specified by the
         $(b,--carcass) option.");
    `I ("4. Default carcass directories", "First the .carcass/flesh files
         from the current working directory up to the root path
         unless the option $(b,--no-dot-dirs) is specified. Then in the
         ~/.carcass/flesh file unless the option $(b,--no-user-dir) is
         specified.");
    `P "The following variables, if undefined by the lookup procedure are
        automatically defined by carcass:";
    `I ("CARCASS_YEAR",
        "Holds the year CE in which the program was started.");
    `I ("CARCASS_MATCH_*",
        "All variables of this form hold the empty string.");
    `P "Depending on the command, remaining undefined variables may be asked
        interactively.";
    `S "BONE LOOKUP";
    `P "If a bone identifier is absolute or starts with './', then the
        corresponding file, if it exists, is read as the bone. If the bone
        identifier is '-' then it is read from standard input. Otherwise
        if the bone identifier is a relative path, the first file matching
        the path in the list of carcass directories is taken as the bone.";
    `P "For a given bone identifier $(i,BONE_ID) the lookup order is the
        following:";
    `I ("1. Command line carcass directories", "Starting from the
         rightmost one, the file $(i,DIR)/$(i,BONE_ID) if it exists
         with $(i,DIR) specified by the $(b,--carcass) option.");
    `I ("2. Default carcass directories", "First the .carcass/$(i,BONE_ID)
         from the current working directory up to the root path
         unless the option $(b,--no-dot-dirs) is specified. Then the the
         ~/.carcass/$(BONE_ID) file unless the option $(b,--no-user-dir) is
         specified.");
    `S "BODY LOOKUP";
    `P "Body lookup works exactly like bone lookup (see above) except that
        it can be specified either by $(i,BODY_ID) or $(i,BODY_ID).body.
        In the first case the '.body' extension is automatically added
        to the requested path identifier before looking up. Note that in
        body files, body identifiers must be specified with the extension
        otherwise they are taken as being bone identifiers."
  ] @ Cli.see_also_main_man

let syntax =
  ("CARCASS-SYNTAX", 5, "", version, carcass_manual),
  [ `S "NAME";
    `P "carcass-syntax - syntax of carcass files";
    `S "DESCRIPTION";
    `P "At the lexical level, carcass files are sequences of keywords
        and $(b,atoms) separated by $(b,white space) and $(b,comments).";
    `P "$(b,variable identifiers) are restricted forms of atoms and
        $(b,variable references) are used to refer to the value of
        variables. Atoms with variable references are called $(b,patterns).";
    `P "We first describe these basic elements before proceding to the
        definition of the syntax of $(b,flesh), $(b,bone) and
        $(b,body) files.";
    `S "CHARACTER STREAM PROCESSING";
    `P "Leaving out binary bone files, carcass only interprets valid
        UTF-8 \ encoded files.";
    `P "If an initial BOM character (U+FEFF) is present it is
        discarded.  In flesh and body files the newline functions CR
        (U+000D), CRLF (<U+000D, U+000A>) and NEL (U+0085) are
        normalized to LF (U+000A). The newlines of bone files are however
        left untouched.";
    `P "The grammar productions below are defined on the resulting
        stream of Unicode scalar values.";
    `S "WHITE SPACE";
    `P "White space is space, horizontal tabulation, line feed, line tabulation
        and form feed. White space delineates atoms and keywords.";
    `Pre "$(i,white) ::= U+0020 | U+0009 | 0x000A | U+000B | U+000C";
    `S "COMMENTS";
    `P "Outside quoted atoms (see below) a hash
        (#, U+0023) and anything that follows is ignored until the next LF
        ($(p,\\)$(g,\\e)n, U+000A) and treated as white space.";
    `Pre "\
$(i,comment) ::= $(i,hash) [^$(i,lf)]* $(i,lf)
     $(i,lf) ::= U+000A
   $(i,hash) ::= U+0023";
    `S "ATOMS";
    `P "An atom is either any sequence of characters except white space
        or a quoted atom, any sequence sequence of characters
        between quotation marks (\", U+0022). For example abc and \"abc\"
        are respectively an atom and a quoted atom and represent the same
        atom.";
    `P "Quoted atoms can be split across lines using a backslash
        ($(p,\\)$(g,\\e), U+005C); in this case initial spaces (U+0020) or
        tabs (U+0009) on the following line are discarded. Quoted atoms can
        also contain white space and escape sequences which are started
        by a backslash character ($(p,\\)$(g,\\e), U+005C). The following
        escape sequences are recognized:";
    `I ("$(p,\\)$(g,\\e)$(p,\\)$(g,\\e)",
        "denotes U+005C, a backslash character"); `Noblank;
    `I ("$(p,\\)$(g,\\e)\"",
        "denotes U+0022, a double quote character"); `Noblank;
    `I ("$(p,\\)$(g,\\e) ",
        "denotes U+0020, a space character"); `Noblank;
    `I ("$(p,\\)$(g,\\e)n",
        "denotes U+000A, a line feed character");
    `P "Any other character following a backslash is an illegal sequence
        of characters.";
    `P "The grammar of atoms is:";
    `Pre "\
\  $(i,atom) ::= [^$(i,aend)]+ | $(i,quote) $(i,qachar)* $(i,quote)
  $(i,aend) ::= $(i,white) | $(i,quote) | $(i,hash)
$(i,qachar) ::= [^$(i,quote) $(i,bslash)] | $(i,bslash) ($(i,bslash) \
              | $(i,quote) | $(i,space) | $(i,n))
 $(i,quote) ::= U+0022
$(i,bslash) ::= U+005C
 $(i,space) ::= U+0020
     $(i,n) ::= U+006E";
    `S "VARIABLE IDENTIFIERS";
    `P "Variable identifiers are sequences of any US-ASCII letter,
        digit or underscore ('_', U+005F). Variable identifiers are case
        insensitive with respect to US-ASCII case maps.";
    `Pre
      "$(i,id) ::= (U+0030-U+0039 | U+0041-U+005A | U+0061-U+007A | U+005F)+";
    `S "VARIABLE REFERENCES";
    `P "Variables references are of the form $$$$(VAR) where VAR is a
        $(b,variable identifier). In the context where variable references \
        are interpreted a literal $$$$ must always be escaped by $$$$$$$$.";
    `P "Variable references can be followed by an optional transform using
        the syntax $$$$(VAR,transform). The following transforms are defined.";
    `I ("$$$$(VAR,uncapitalize)", "Uncapitalizes the first letter of VAR's
        definition according to US-ASCII case maps.");
    `I ("$$$$(VAR,capitalize)", "Capitalizes the first letter of VAR's
        definition according to US-ASCII case maps.");
    `I ("$$$$(VAR,lowercase)", "Lowercases the letters of VAR's
        definition according to US-ASCII case maps.");
    `I ("$$$$(VAR,uppercase)", "Uppercases the letters of VAR's
        definition according to US-ASCII case maps.");
    `I ("$$$$(VAR,indent(ATOM))", "Prefixes each line of VAR's
        definition with the string ATOM. Lines that result in whitespace
        only are collapsed to an empty line.");
    `P "In context where variable references need to be recognized they
        are according to the following grammar.";
    `Pre "\
\      $(i,ref) ::= $(i,dollar) $(i,lpar) $(i,refc) $(i,rpar)
     $(i,refc) ::= $(i,id) | $(i,id) $(i,comma) $(i,transform)
$(i,transform) ::= $(i,id) [ $(i,lpar) $(i,atom) $(i,rpar) ]
   $(i,dollar) ::= U+0024
    $(i,comma) ::= U+002C
     $(i,lpar) ::= U+0028
     $(i,rpar) ::= U+0029";
    `S "PATTERNS";
    `P "Patterns are atoms (quoted or not) in which variable references are
        recognized";
    `Pre "$(i,pat) ::= $(i,atom)";
    `S "FLESH FILES";
    `P "Flesh files are sequences of variable definitions. A variable
        definition is a variable identifier and a pattern defining
        the variable value.";
    `Pre "\
$(i,flesh) ::= ($(i,id) $(i,pat))*
";
    `S "BONE FILES";
    `P "Bone files are either UTF-8 encoded files in which variable references
        are recognized or binary files (detected by the presence of a NULL
        byte) which are arbitrary, uninterpreted, sequence of bytes.";
    `S "BODY FILES";
    `P "Body files start with a documentation directive, followed
        by a sequence of variable documentation directives and end
        with a sequence of bind directives.";
    `P "The body documentation directive 'doc' specifies a synopsis and
        long description for the body.";
    `P "A variable documentation directive 'var' specifies documentation
        for a variable identifier (used for interactive variable
        definition).";
    `P "A bind directive 'bind' specifies a relative path as a pattern
        and a bone or body identifier to bind to the path's content. Note
        that unlike command line tool arguments these identifiers cannot
        be absolute or start with ./, and body identifiers have to be
        specified with their .body exentension otherwise they are taken
        to be bone identifiers.";
    `Pre "\
$(i,body) ::= $(i,doc) $(i,var)* $(i,bind)*
 $(i,doc) ::= \"doc\" $(i,atom) $(i,atom)
 $(i,var) ::= \"var\" $(i,id) $(i,atom)
$(i,bind) ::= \"bind\" $(i,pat) $(i,atom)";
 ] @ Cli.see_also_main_man

(* Help command *)

let pages =
  [ "basics", basics;
    "lookup", lookup;
    "syntax", syntax; ]

let help man_format topic commands = match topic with
| None -> `Help (man_format, None)
| Some topic ->
    let topics = "topics" :: commands @ (List.map fst pages) in
    let topics = List.sort compare topics in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when List.mem t commands -> `Help (man_format, Some t)
    | `Ok t when t = "topics" ->
        Fmt.pr "@[<v>%a@]@." Fmt.(list string) topics;
        `Ok 0
    | `Ok t ->
        let man = try List.assoc t pages with Not_found -> assert false in
        Fmt.pr "%a" (Cmdliner.Manpage.print man_format) man;
        `Ok 0

(* Command line interface *)

open Cmdliner

let topic =
  let doc = "The topic to get help on, `topics' lists the topic." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)

let doc = "show help about carcass"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command shows help about carcass.";
    `P "Use `topics' as $(i,TOPIC) to get a list of topics.";
  ] @ Cli.see_also_main_man

let cmd =
  let info = Term.info "help" ~doc ~man in
  let t = Term.(ret (const help $ Term.man_format $ topic $
                      Term.choice_names))
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

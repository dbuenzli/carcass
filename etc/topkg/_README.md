$(NAME) — $(PKG_SYNOPSIS)
-------------------------------------------------------------------------------
%%VERSION%%

$(NAME) is TODO

$(NAME) is distributed under the $(LICENSE) license.

Homepage: $(PKG_HOMEPAGE)  

## Installation

$(NAME) can be installed with `opam`:

    opam install $(NAME,uncapitalize)

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
$(NAME,uncapitalize)`.

[doc]: $(PKG_DOC)

## Sample programs

If you installed $(NAME) with `opam` sample programs are located in
the directory `opam var $(NAME,uncapitalize):doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built and run
with:

    topkg build --tests true && topkg test 

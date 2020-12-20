TEST=test/test.exe
MAIN=bin/main.exe
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	dune utop lib

build:
	dune build $(MAIN)

buildtest:
	dune build $(TEST)

test: buildtest
	./_build/default/test/test.exe

interface: build
	./_build/default/bin/main.exe cli

server: build
	./_build/default/bin/main.exe

zip:
	zip -r ms3.zip bin lib test _tags .merlin .ocamlinit dune dune-project geotag-db.opam Makefile INSTALL.md README.md

docs: build
	dune build @doc

clean:
	dune clean
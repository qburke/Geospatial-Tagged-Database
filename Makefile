MODULES=rtree point rect db command state entry regression database_test rect_test
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
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
	zip -r ms3.zip bin lib test _tags .merlin .ocamlinit dune Makefile INSTALL.md README.md

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,dolog \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	dune build @doc-private

clean:
	dune clean
	rm -rf doc.public 

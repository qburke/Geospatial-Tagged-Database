MODULES=rtree point rect db command state entry regression database_test rect_test
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=bin/main.exe
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	dune utop lib

build:
	dune build $(MAIN)

test:
	dune runtest test

interface: build
	./_build/default/bin/main.exe

zip:
	zip -r ms3.zip bin lib test _tags .merlin .ocamlinit dune Makefile INSTALL.md README.md

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,dolog \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,dolog \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	dune clean
	rm -rf doc.public doc.private

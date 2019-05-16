MODULES=cats main command state authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,qcheck,yojson,ANSITerminal

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private a7.zip _build $(MAIN) $(TEST)

zip:
	zip a7.zip *.ml* *.json _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

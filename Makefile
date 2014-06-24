ocamlbuild = ocamlbuild -use-ocamlfind -syntax camlp4o -pkg js_of_ocaml -pkg js_of_ocaml.syntax

main:
	$(ocamlbuild) main.native

js_byte:
	$(ocamlbuild) main_js.byte

js: js_byte
	js_of_ocaml main_js.byte -I . -file stdlib.sh:/

.PHONY : clean
clean:
	ocamlbuild -clean

.PHONY : test
test: main
	tests/runtests.sh

all: clean main test

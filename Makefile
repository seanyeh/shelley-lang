all: main test

main:
	ocamlbuild main.native

.PHONY : clean
clean:
	ocamlbuild -clean

.PHONY : test
test: main
	tests/runtests.sh


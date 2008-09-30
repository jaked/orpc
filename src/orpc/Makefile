FILES=\
orpc.cma orpc.cmxa orpc.a \
orpc.mli orpc.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild orpc.cma orpc.cmxa

install:
	ocamlfind install orpc META $(BFILES)

uninstall:
	ocamlfind remove orpc

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

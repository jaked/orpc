FILES=\
orpc.cma orpc.cmxa orpc.a orpc.cmjsa \
orpc.mli orpc.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild orpc.cma orpc.cmxa orpc.cmjsa
	ocamlfind remove -destdir ../../stage orpc
	ocamlfind install -destdir ../../stage orpc META $(BFILES)

install:
	ocamlfind install orpc META $(BFILES)

uninstall:
	ocamlfind remove orpc

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

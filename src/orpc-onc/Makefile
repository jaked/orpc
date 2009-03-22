-include ../../Makefile.conf

FILES=\
orpc-onc.cma orpc-onc.cmxa orpc-onc.a \
orpc_onc.mli orpc_onc.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild orpc-onc.cma orpc-onc.cmxa
	ocamlfind remove -destdir ../../stage orpc-onc
	ocamlfind install -destdir ../../stage orpc-onc META $(BFILES)

install:
	ocamlfind install orpc-onc META $(BFILES)

uninstall:
	ocamlfind remove orpc-onc

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

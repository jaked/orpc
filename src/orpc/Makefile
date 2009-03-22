-include ../../Makefile.conf

ifeq ($(ENABLE_OCAMLJS),1)
CMJSA=orpc.cmjsa
endif

FILES=\
orpc.cma orpc.cmxa orpc.a $(CMJSA) \
orpc.mli orpc.cmi \
orpc_pp.mli orpc_pp.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild orpc.cma orpc.cmxa $(CMJSA)
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

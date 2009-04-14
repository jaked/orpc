-include ../../Makefile.conf

FILES=\
orpc-onc.cma orpc-onc.cmxa orpc-onc.a \
orpc_onc.mli orpc_onc.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild orpc-onc.cma orpc-onc.cmxa

doc:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild -no-links doc.docdir/index.html

install:
	ocamlfind install orpc-onc META $(BFILES)

uninstall:
	ocamlfind remove orpc-onc

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

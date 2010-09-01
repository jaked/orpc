-include ../../Makefile.conf

FILES=\
orpc-js-comet-server.cma orpc-js-comet-server.cmxa orpc-js-comet-server.a \
orpc_js_comet_server.mli orpc_js_comet_server.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild orpc-js-comet-server.cma orpc-js-comet-server.cmxa

doc:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild -no-links doc.docdir/index.html

install:
	ocamlfind install orpc-js-comet-server META $(BFILES)

uninstall:
	ocamlfind remove orpc-js-comet-server

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

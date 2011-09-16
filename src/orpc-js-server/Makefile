-include ../../Makefile.conf

FILES=\
orpc-js-server.cma orpc-js-server.cmxa orpc-js-server.a \
orpc_js_server.mli orpc_js_server.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild $(OCAMLBUILD_FLAGS) orpc-js-server.cma orpc-js-server.cmxa
	ocamlfind remove -destdir ../../stage orpc-js-server
	ocamlfind install -destdir ../../stage orpc-js-server META $(BFILES)

doc:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild -no-links doc.docdir/index.html

install:
	ocamlfind install orpc-js-server META $(BFILES)

uninstall:
	ocamlfind remove orpc-js-server

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

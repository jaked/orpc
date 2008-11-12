FILES=\
orpc-js-server.cma orpc-js-server.cmxa \
orpc_js_server.mli orpc_js_server.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild orpc-js-server.cma orpc-js-server.cmxa
	ocamlfind remove -destdir ../../stage orpc-js-server
	ocamlfind install -destdir ../../stage orpc-js-server META $(BFILES)

install:
	ocamlfind install orpc-js-server META $(BFILES)

uninstall:
	ocamlfind remove orpc-js-server

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

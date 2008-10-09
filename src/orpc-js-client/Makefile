FILES=\
orpc-js-client.cmjsa \
orpc_js_client.mli orpc_js_client.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild orpc-js-client.cmjsa
	ocamlfind remove -destdir ../../stage orpc-js-client
	ocamlfind install -destdir ../../stage orpc-js-client META $(BFILES)

install:
	ocamlfind install orpc-js-client META $(BFILES)

uninstall:
	ocamlfind remove orpc-js-client

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

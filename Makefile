FILES=\
orpc_lib.cma orpc_lib.cmxa orpc_lib.a \
orpc_xdr.mli orpc_xdr.cmi

BFILES=$(addprefix _build/,$(FILES))

all:
	ocamlbuild orpc.native orpc_lib.cma orpc_lib.cmxa

install: all
	ocamlfind install orpc META $(BFILES)
	cp orpc.native /usr/local/bin/orpc

uninstall:
	ocamlfind remove orpc

clean:
	ocamlbuild -clean

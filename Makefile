all:
	ocamlbuild orpc.native

install:
	cp orpc.native /usr/local/bin/orpc

clean:
	ocamlbuild -clean

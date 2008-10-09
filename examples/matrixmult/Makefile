all: myocamlbuild.ml
	ocamlbuild mm_client.byte mm_server.byte

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

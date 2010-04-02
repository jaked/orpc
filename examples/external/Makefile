all: myocamlbuild.ml
	ocamlbuild client.byte server.byte

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

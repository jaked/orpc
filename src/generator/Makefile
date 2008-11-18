BINDIR=`ocamlfind printconf stdlib`/../../bin

all:
	ocamlbuild main.native

install: all
	cp _build/main.native $(BINDIR)/orpc

uninstall:
	rm -f $(BINDIR)/orpc

clean:
	ocamlbuild -clean

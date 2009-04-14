-include ../../Makefile.conf

all:
	ocamlbuild main.native

doc:

install: all
	cp _build/main.native $(BINDIR)/orpc

uninstall:
	rm -f $(BINDIR)/orpc

clean:
	ocamlbuild -clean

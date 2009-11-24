-include ../../Makefile.conf

FILES=\
orpc_generator.cmo \
pa_orpc_with.cmo \
pa_orpc_with.mli pa_orpc_with.cmi \

BFILES=$(addprefix _build/,$(FILES))

ifeq ($(ENABLE_TYPE_CONV),1)
PA_CMO=pa_orpc_with.cmo
endif

all: myocamlbuild.ml
	ocamlbuild main.byte $(PA_CMO)

doc:

install:
	if [ $(ENABLE_TYPE_CONV) = 1 ]; then ocamlfind install orpc-syntax META $(BFILES); fi
	install _build/main.byte $(BINDIR)/orpc

uninstall:
	if [ $(ENABLE_TYPE_CONV) = 1 ]; then ocamlfind remove orpc-syntax; fi
	rm -f $(BINDIR)/orpc

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

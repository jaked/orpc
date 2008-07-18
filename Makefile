FILES=\
orpc.cma orpc.cmxa orpc.a \
orpc.mli orpc.cmi \

BFILES=$(addprefix _build/,$(FILES))

LWT_FILES=\
lwt-equeue.cma lwt-equeue.cmxa lwt-equeue.a \
lwt.mli lwt.cmi \
lwt_mutex.mli lwt_mutex.cmi \
lwt_unix.mli lwt_unix.cmi \
lwt_util.mli lwt_util.cmi \
lwt_timeout.mli lwt_timeout.cmi \

LWT_BFILES=$(addprefix _build/lwt-equeue/,$(LWT_FILES))

all:
	ocamlbuild main.native orpc.cma orpc.cmxa lwt-equeue/lwt-equeue.cma lwt-equeue/lwt-equeue.cmxa

install: all
	ocamlfind install orpc META $(BFILES)
	ocamlfind install lwt-equeue lwt-equeue/META $(LWT_BFILES)
	cp main.native /usr/local/bin/orpc

uninstall:
	ocamlfind remove orpc
	ocamlfind remove lwt-equeue

clean:
	ocamlbuild -clean

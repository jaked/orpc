FILES=\
orpc.cma orpc.cmxa orpc.a \
orpc.mli orpc.cmi \

BFILES=$(addprefix _build/src/orpc/,$(FILES))

LWT_FILES=\
lwt-equeue.cma lwt-equeue.cmxa lwt-equeue.a \
lwt.mli lwt.cmi \
lwt_mutex.mli lwt_mutex.cmi \
lwt_pool.mli lwt_pool.cmi \
lwt_timeout.mli lwt_timeout.cmi \
lwt_unix.mli lwt_unix.cmi \
lwt_util.mli lwt_util.cmi \

LWT_BFILES=$(addprefix _build/src/lwt-equeue/,$(LWT_FILES))

all:
	ocamlbuild src/generator/main.native src/orpc/orpc.cma src/orpc/orpc.cmxa src/lwt-equeue/lwt-equeue.cma src/lwt-equeue/lwt-equeue.cmxa

install: all
	ocamlfind install orpc src/orpc/META $(BFILES)
	ocamlfind install lwt-equeue src/lwt-equeue/META $(LWT_BFILES)
	cp _build/src/generator/main.native `ocamlfind printconf stdlib`/../../bin/orpc

uninstall:
	ocamlfind remove orpc
	ocamlfind remove lwt-equeue

clean:
	ocamlbuild -clean

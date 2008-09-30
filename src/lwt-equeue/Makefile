FILES=\
lwt-equeue.cma lwt-equeue.cmxa lwt-equeue.a \
lwt.mli lwt.cmi \
lwt_mutex.mli lwt_mutex.cmi \
lwt_pool.mli lwt_pool.cmi \
lwt_timeout.mli lwt_timeout.cmi \
lwt_unix.mli lwt_unix.cmi \
lwt_util.mli lwt_util.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild lwt-equeue.cma lwt-equeue.cmxa

install:
	ocamlfind install lwt-equeue META $(BFILES)

uninstall:
	ocamlfind remove lwt-equeue

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

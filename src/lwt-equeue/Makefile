-include ../../Makefile.conf

FILES=\
lwt-equeue.cma lwt-equeue.cmxa lwt-equeue.a \
lwt.mli lwt.cmi \
lwt_chan.mli lwt_chan.cmi \
lwt_condition.mli lwt_condition.cmi \
lwt_mutex.mli lwt_mutex.cmi \
lwt_pool.mli lwt_pool.cmi \
lwt_pqueue.mli lwt_pqueue.cmi \
lwt_queue.mli lwt_queue.cmi \
lwt_timeout.mli lwt_timeout.cmi \
lwt_unix.mli lwt_unix.cmi \
lwt_util.mli lwt_util.cmi \
lwt_equeue.mli lwt_equeue.cmi

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild lwt-equeue.cma lwt-equeue.cmxa

doc:
	ocamlbuild -no-links doc.docdir/index.html

install:
	ocamlfind install lwt-equeue META $(BFILES)

uninstall:
	ocamlfind remove lwt-equeue

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .

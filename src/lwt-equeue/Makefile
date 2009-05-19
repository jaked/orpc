-include ../../Makefile.conf

ifeq ($(ENABLE_SSL),1)
SSL_LIBS=lwt_ssl.cma lwt_ssl.cmxa
SSL_FILES=\
lwt_ssl.a \
lwt_ssl.mli lwt_ssl.cmi
endif

LIBS=\
lwt.cma lwt.cmxa \
lwt_preemptive.cma lwt_preemptive.cmxa \
lwt_extra.cma lwt_extra.cmxa \
$(SSL_LIBS)

FILES=\
$(LIBS) \
lwt.a \
lwt_preemptive.a \
lwt_extra.a \
lwt.mli lwt.cmi \
lwt_chan.mli lwt_chan.cmi \
lwt_lib.mli lwt_lib.cmi \
lwt_mutex.mli lwt_mutex.cmi \
lwt_pool.mli lwt_pool.cmi \
lwt_preemptive.mli lwt_preemptive.cmi \
lwt_timeout.mli lwt_timeout.cmi \
lwt_unix.mli lwt_unix.cmi \
lwt_util.mli lwt_util.cmi \
lwt_equeue.mli lwt_equeue.cmi \
lwt_condition.mli lwt_condition.cmi \
lwt_queue.mli lwt_queue.cmi \
lwt_pqueue.mli lwt_pqueue.cmi \
lwt_event.mli lwt_event.cmi \
lwt_event_unix.mli lwt_event_unix.cmi \
lwt_event_queue.mli lwt_event_queue.cmi \
$(SSL_FILES)

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild $(LIBS)

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

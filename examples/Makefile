-include ../Makefile.conf

DIRS=matrixmult modules modules_async modules_lwt simple simple_async

ifeq ($(ENABLE_OCAMLJS),1)
DIR=$(DIRS) clicks
endif

all:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir all || exit; \
	done

clean:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir clean || exit; \
	done

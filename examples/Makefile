-include ../Makefile.conf

DIRS=matrixmult modules simple

ifeq ($(ENABLE_OCAMLJS),1)
DIRS += clicks
endif

all:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir all || exit; \
	done

clean:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir clean || exit; \
	done

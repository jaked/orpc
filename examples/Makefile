-include ../Makefile.conf

DIRS=matrixmult modules

ifeq ($(ENABLE_OCAMLJS),1)
  DIRS += clicks

  ifeq ($(ENABLE_COHTTPSERVER),1)
    DIRS += clicks-cohttp clicks-comet
  endif
endif

ifeq ($(ENABLE_TYPE_CONV),1)
  DIRS += external
endif

all:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir all || exit; \
	done

clean:
	for dir in $(DIRS); do \
		$(MAKE) -C $$dir clean || exit; \
	done

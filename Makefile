-include Makefile.conf

all:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg all || exit; \
	done

install:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg install || exit; \
	done

uninstall:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg uninstall || exit; \
	done

clean:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg clean || exit; \
	done
	make -C examples clean
	rm -rf stage/*

distclean: clean
	rm -rf Makefile.conf

examples:
	make -C examples

.PHONY: examples

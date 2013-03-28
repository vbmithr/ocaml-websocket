#OBUILDOPTS="--debug+"
PACKAGE_NAME=websocket
SHELL=bash

FINDLIB_SUFFIXES := cmi cmx cma cmxa o a

.PHONY: clean uninstall

all: dist/setup/lib-$(PACKAGE_NAME)

dist/setup:
	obuild $(OBUILDOPTS) configure

dist/setup/lib-$(PACKAGE_NAME): dist/setup
	obuild $(OBUILDOPTS) build

install: dist/setup/lib-$(PACKAGE_NAME)
	ocamlfind remove $(PACKAGE_NAME)
	ocamlfind install $(PACKAGE_NAME) $(addprefix dist/build/lib-$(PACKAGE_NAME)/$(PACKAGE_NAME)., $(FINDLIB_SUFFIXES)) lib/META

clean:
	obuild clean

uninstall:
	ocamlfind remove $(PACKAGE_NAME)

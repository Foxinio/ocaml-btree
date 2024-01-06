
all:
	dune build

install:
	dune install

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean

.PHONY: all install uninstall test clean

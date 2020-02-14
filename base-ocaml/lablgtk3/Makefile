.PHONY: build build-all nopromote opam clean

build:
	dune build

# This also builds examples
build-all:
	dune build @all

nopromote:
	dune build @all --ignore-promoted-rules

# We first pin lablgtk3 as to avoid problems with parallel make
opam:
	opam pin add lablgtk3 . --kind=path -y
	opam install lablgtk3

clean:
	dune clean

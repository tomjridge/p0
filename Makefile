SHELL:=bash

build:
	dune build 
	dune build bin/p0_example.exe

install:
	dune install

run_example:
	dune exec ./bin/p0_example.exe

docs: FORCE
	dune build @doc

copy_docs: FORCE
	cp -R _build/default/_doc/_html/* docs

clean:
	rm -rf *.{cmi,cmo,cmx,o,a,cmxa,cma} a.out _build
	dune clean
	rm -f p0_lib.install

FORCE:

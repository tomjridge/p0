SHELL:=bash

build:
	dune build 
	dune build bin/p0_example.exe

install:
	dune install

run_example:
	dune exec ./bin/p0_example.exe

clean:
	rm -rf *.{cmi,cmo,cmx,o,a,cmxa,cma} a.out _build
	dune clean
	rm -f p0_lib.install

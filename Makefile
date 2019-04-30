SHELL:=bash

build:
	dune build @install
#	dune build src/re_test.exe
#	_build/default/src/re_test.exe
#	dune build bin/p0_example.exe

install:
	dune install

run_example:
	dune exec ./bin/p0_example.exe

BUILD_DOC:=_build/default/_doc/_html
docs: FORCE
	dune build @doc
	mkdir -p /tmp/p0
	rsync -vaz $(BUILD_DOC)/* /tmp/p0

promote_docs: FORCE
	rm -rf docs/*
	cp -R $(BUILD_DOC)/* docs

clean:
	rm -rf *.{cmi,cmo,cmx,o,a,cmxa,cma} a.out _build
	dune clean
	rm -f p0_lib.install

FORCE:

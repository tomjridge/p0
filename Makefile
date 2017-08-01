libname=p0
all:
	ocamlfind ocamlc -c p0.ml
	ocamlfind ocamlopt -c p0.ml
	ocamlfind ocamlc -g -a -o $(libname).cma p0.cmo
	ocamlfind ocamlopt -g -a -o $(libname).cmxa p0.cmx
	-ocamlfind remove $(libname)
	ocamlfind install $(libname) META *.cmi *.o *.a *.cma *.cmxa *.cmo *.cmx 

clean:
	rm -f *.{cmi,cmo,cmx,o,a,cmxa,cma}


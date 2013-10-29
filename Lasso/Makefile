# Default 
LIB=-I,$(shell ocamlfind query lacaml)


all: 
	ocamlbuild -use-ocamlfind -pkgs bigarray,lacaml,str,unix -verbose 2 test_lasso.native

clean: 
	ocamlbuild -clean



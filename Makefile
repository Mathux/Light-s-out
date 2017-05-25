all:
	ocamlbuild  -tag thread -use-ocamlfind -package graphics -package unix lights_out.native
	mv lights_out.native lights_out
clean:
	ocamlbuild -clean

all:
	ocamlbuild -use-ocamlfind -package graphics -package unix Algo.native
	mv Algo.native Algo
clean:
	ocamlbuild -clean

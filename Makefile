all:
	ocamlbuild -use-ocamlfind -package graphics Algo.native
	mv Algo.native Algo
clean:
	ocamlbuild -clean

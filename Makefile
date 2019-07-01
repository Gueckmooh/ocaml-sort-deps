all:
	ocamlbuild -use-ocamlfind -package str -I src/ parse_file.byte

clean:
	ocamlbuild -clean

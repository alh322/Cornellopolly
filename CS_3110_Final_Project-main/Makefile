.PHONY: test
.PHONY: play
.PHONY: doc

build:
	dune build

play:
	dune exec bin/mainiter.exe

clean:
	dune clean

test:
	dune exec test/main.exe
doc:
	dune build @doc

-include OCamlMakefile
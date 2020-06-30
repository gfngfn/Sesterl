all:
	dune build src/main.exe
	cp _build/default/src/main.exe ./sesterl

.PHONY: test
test:
	dune exec test/testRange.exe

.PHONY: clean
clean:
	dune clean

all:
	dune build src/main.exe
	cp _build/default/src/main.exe ./sesterl

.PHONY: test
test:
	dune exec test/testRange.exe

.PHONY: test-positive
test-positive:
	./run-test-to-pass.sh

.PHONY: clean
clean:
	dune clean

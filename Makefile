all:
	dune build src/main.exe
	cp _build/default/src/main.exe ./sesterl

.PHONY: test
test: test-unit test-positive

.PHONY: test-unit
test-unit:
	dune exec test/testRange.exe
	dune exec test/testIdentifierScheme.exe

.PHONY: test-positive
test-positive:
	./run-test-to-pass.sh

.PHONY: clean
clean:
	dune clean

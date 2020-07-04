all:
	dune build src/main.exe
	cp _build/default/src/main.exe ./sesterl

.PHONY: test
test: test-blackbox-positive test-blackbox-negative test-unit

.PHONY: test-unit
test-unit:
	dune exec test/testRange.exe
	dune exec test/testIdentifierScheme.exe

.PHONY: test-blackbox-positive
test-blackbox-positive:
	./run-positive-blackbox-tests.sh

.PHONY: test-blackbox-negative
test-blackbox-negative:
	./run-negative-blackbox-tests.sh

.PHONY: clean
clean:
	dune clean

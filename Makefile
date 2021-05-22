.PHONY: all
all:
	dune build -p sesterl
	cp _build/default/src/main.exe ./sesterl

.PHONY: test
test: test-blackbox-positive test-blackbox-negative test-unit

.PHONY: test-unit
test-unit:
	dune exec test/testRange.exe
	dune exec test/testIdentifierScheme.exe

.PHONY: test-blackbox-positive
test-blackbox-positive: submodule
	./run-positive-blackbox-tests.sh

.PHONY: test-blackbox-negative
test-blackbox-negative: submodule
	./run-negative-blackbox-tests.sh

.PHONY: submodule
submodule:
	git submodule update --init --recursive

.PHONY: clean
clean:
	dune clean

.PHONY: clean-test
clean-test:
	rm -f test/_generated/*

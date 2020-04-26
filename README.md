# [WIP] Sesterl: A Session-Typed Erlang

## TODO

* [ ] Message-passing primitives
  * [x] `spawn`
  * [x] `receive`-expressions
  * [x] `send`
  * [x] `self`
  * [ ] `monitor`/`demonitor`
  * [ ] `link`/`unlink`
* [ ] Type annotation
* [ ] Output Erlang code
* [ ] FFI
* [ ] Product types
* [ ] Data types
  * [ ] Strings
  * [ ] Binaries
  * [ ] Lists
  * [ ] Records
  * [ ] User-defined ADTs
  * [ ] Pattern matching
* [ ] Mutual recursion by generalized `letrec`-expressions
* [ ] Pattern matching by generalized `let`-expressions

## Build Notes

### Create a local switch

```bash
opam switch create .
```

### Install the required packages

```bash
opam install . --deps-only --with-doc --with-test
```

### Build

```bash
make
```

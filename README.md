# [WIP] Sesterl: A Session-Typed Erlang

## TODO

* [ ] Message-passing primitives
  * [x] `spawn`
  * [x] `receive`-expressions
  * [x] `send`
  * [x] `self`
  * [ ] `monitor`/`demonitor`
  * [ ] `link`/`unlink`
* [x] Principal type inference
* [ ] Type annotation
* [x] Output Erlang code
* [ ] FFI
* [ ] Data types
  * [ ] Strings/Binaries
  * [x] Product types
  * [x] Lists
  * [ ] Records
  * [x] User-defined ADTs
  * [x] Pattern matching
* [ ] Mutual recursion by generalized `letrec`-expressions
* [x] Pattern matching by generalized `let`-expressions

## References

* Simon Fowler. *Typed Concurrent Functional Programming with Channels, Actors, and Sessions*. PhD thesis, University of Edinburgh, 2019.
* Dominic Orchard and Nobuko Yoshida. Effects as sessions, sessions as effects. In *Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL’16)*, pp. 568–581, 2016.

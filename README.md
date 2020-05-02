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
* [x] Type annotation
* [x] Output Erlang code
* [ ] FFI
* [ ] Data types
  * [x] Strings/Binaries
  * [x] Product types
  * [x] Lists
  * [x] User-defined ADTs
  * [x] Pattern matching
  * [ ] Type synonyms
  * [ ] Records
* [x] Mutual recursion by generalized `letrec`-expressions
* [x] Pattern matching by generalized `let`-expressions
* [ ] Module system
* [ ] (Multiparty) session types

## References

* Simon Fowler. *Typed Concurrent Functional Programming with Channels, Actors, and Sessions*. PhD thesis, University of Edinburgh, 2019.
* Dominic Orchard and Nobuko Yoshida. Effects as sessions, sessions as effects. In *Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL’16)*, pp. 568–581, 2016.
* Andreas Rossberg, Claudio Russo, and Derek Dreyer. F-ing modules. *Journal of Functional Programming*, **24**(5), pp. 529–607, 2014.

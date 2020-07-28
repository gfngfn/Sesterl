# [WIP] Sesterl: A Session-Typed Erlang

## Summary

*Sesterl* (pronounced as /səsˈtɚːl/) is a statically-typed functional language that is intended to compile to Erlang.

Contrary to its name, Sesterl has not supported session types yet; it only checks the type of messages every process can receive.

As mentioned in the section “Features” below, however, many features as a typed functional language have already been furnished. Among them are the following:

* First-class higher-order functions
* ADTs and pattern matching
* The standard *Damas–Milner polymorphism* (i.e. so-called the *let-polymorphism*) and *Hindley–Milner type inference* \[Hindley 1969\]\[Milner 1978\]
* Type-level distinction between pure calculations and concurrent computations by a kind of monads \[Fowler 2019\]
* A module system equipped with functors based on *F-ing modules* \[Rossberg, Russo & Dreyer 2014\]


## Table of contents

- [How to install](#how-to-install)
- [How to build source files for development](#how-to-build-source-files-for-development)
- [Features](#features)
  - [Function definition](#function-definition)
  - [Polymorphism](#polymorphism)
  - [ADTs](#adts)
  - [Pattern matching](#pattern-matching)
  - [Concurrency](#concurrency)
  - [Module system](#module-system)
  - [FFI](#ffi)
- [Future work](#future-work)
  - [TODO list](#todo-list)
- [References](#references)


## How to install

Under the condition that Dune (≥ 2.5) and OPAM are installed, invoke:

```console
$ git clone https://github.com/gfngfn/Sesterl.git
$ cd Sesterl
$ opam pin add sesterl .
  # Probably this command asks you whether to install the package (and its dependencies).
  # You may answer Y to do so.
$ sesterl --version
```


## How to build source files for development

Under the condition that Dune (≥ 2.5) and Make are installed, invoke:

```
$ make
```


## Features

Sesterl provides many ML-inspired features (i.e. basically resembles OCaml, Standard ML, F\#, Reason, etc.).


### Function definition

Function definition is performed by `let`-expressions:

```
let add(x, y) = x + y
```

Unlike ML family, however, in order to realize seemless compilation to top-level function definitions in Erlang, functions have their own arity (i.e. not curried by nature) and thereby have types of the form `fun(τ_1, …, τ_n) -> τ`. The function `add` defined above, for instance, has type `fun(int, int) -> int`, which is **NOT** equivalent to `fun(int) -> (fun(int) -> int)`.

Incidentally, you do not have to annotate types of arguments or return values; they will be reconstructed by standard *Hindley–Milner type inference*. you can nonetheless add type annotation like the following:

```
let add(x : int, y : int) : int = x + y
```

You can define higher-order functions, of course:

```
let apply(f, x) = f(x)
```

As is the case in ML, `apply` has a polymorphic type. Features related to type polymorphism is explained later.

Recursive or mutually recursive functions can be defined by using `letrec`-expressions, not only globally but also in a local scope:

```
letrec fact(n) =
  if n <= 0 then 1 else n * fact(n - 1)

let is_even_nat(n) =
  letrec odd(n) =
    if n == 0 then false else even(n - 1)

  andrec even(n) =
    if n == 0 then true else odd(n - 1)
  in
  if n < 0 then false else even(n)
```

Note that, unlike Erlang, function names are all lowercased regardless of whether they are defined in the global scope or in a local one. You can also write, for example, `apply(fact, 6)`; each name of globally-defined functions can be used for the function value the name is bound to, just as locally defined function names can be. This is different from the situation in Erlang, where a globally-defined function name by itself will be interpreted as an atom of the same text.


### Polymorphism

Values defined by `let` or `letrec` can be polymorphic. For instance, the function `proj1` defined as follows has type `<$a, $b> fun($a, $b) -> $a` (where `<$a, $b>` stands for universal quantification):

```
let proj1(x, y) = x
```

Instead of relying upon type inference, you can also annotate polymorphic types and check that the defined function is indeed polymorphic:

```
let proj1<$a, $b>(x : $a, y : $b) : $a = x
```


### ADTs

You can define (non-generalized) algebraic data types and type synonyms in a standard way like the following:

```
type name = binary

type with_number<$a> = ($a, int)

type option<$a> =
  | None
  | Some($a)

type bintree<$b> =
  | Node($b, bintree<$b>, bintree<$b>)
  | Empty
```

Here, `($a, int)` is an example use of standard product types.

As can be seen from the example above, type names start with a lowercase letter, constructors do with an uppercase one, and type variables are denoted by using a preceding `$`.

Each application of a constructor `Ctor(e_1, …, e_n)` will be compiled to a tuple `{ctor, e_1, …, e_n}` in Erlang where `ctor` is a lowercased atom corresponding to `Ctor`.

List-generating constructors, `[]` (nil) and `::` (cons), are also supported by default, of course.


### Pattern matching

You can decompose values of ADTs by using `case`-expression in an ordinary way like the following:

```
let reverse<$a>(xs : list<$a>) : list<$a> =
  letrec aux(acc, xs) =
    case xs of
    | []        -> acc
    | x :: tail -> aux(x :: acc, tail)
    end
  in
  aux([], xs)

letrec tree_size(t : bintree<$a>) =
  case t of
  | Empty           -> 0
  | Node(_, t1, t2) -> 1 + tree_size(t1) + tree_size(t2)
  end
```


### Concurrency

As in Erlang, you can use primitives `self`, `send`, and `spawn` for message-passing concurrency. They are given types by using a kind of monadic types `[τ_0]τ_1` and types `pid<τ>` for PIDs (i.e. process identifiers) as follows:

* `self<$p> : [$p]pid<$p>`
* `send<$p, $q> : fun(pid<$q>, $q) -> [$p]unit`
* `spawn<$p, $q> : fun([$q]unit) -> [$p]pid<$q>`

This formalization is based on *λ\_\{act\}* \[Fowler 2019\].

Intuitively, `[τ_0]τ_1` is the type for suspended concurrent computations that will be run on processes capable of receiving messages of type `τ_0` and that finally produce a value of type `τ_1`. The composition of such computations can be done by `do`-notation. Receiving messages can be done by using `receive`-expressions. See a small example below:

```
let pickup(pid, pids) =
  /* Implementation omitted.
     Finds `pid` from `pids` and returns the rest wrapped by `Some(_)`.
     Returns `None` if `pid` was not found. */

letrec wait_all(acc, pids) =
  case pids of
  | [] ->
      return(acc)

  | _ :: _ ->
      receive
      | (from, msg) ->
          case pickup(from, pids) of
          | None       -> /* unexpected message has arrived */
          | Some(rest) -> wait_all(msg :: acc, rest)
          end
      end
  end

letrec spawn_all(acc, n) =
  if n <= 0 then
    return(acc)
  else
    do parent <- self in
    do pid <- spawn(
      do me <- self in
      let msg = some_heavy_calculation(n) in
      send(parent, (me, msg))
    ) in
    spawn_all(pid :: acc, n - 1)

let main() =
  let n = 10 in
  do pids <- spawn_all([], n) in
  do msgs <- wait_all([], pids) in
  …
```

Here, the primitive `return<$p, $a> : fun($a) -> [$p]$a` lifts a pure value to the computation that has no effect and simply returns the value.

The function `spawn_all` takes an integer `n`, spawns `n` processes that perform some heavy calculation in parallel, and returns their PIDs. `wait_all`, on the other hand, waits all the messages sent from the processes spawned by `spawn_all` and makes a list from the messages. These functions are typed as follows, supposing `some_heavy_calculation` is of type `fun(int) -> answer`:

* `spawn_all<$p, $q> : fun(list<pid<$q>>, int) -> [$p]list<pid<$q>>`
* `wait_all<$q> : fun(list<answer>, list<pid<$q>>) -> [(pid<$q>, answer)]list<answer>`

As mentioned earlier, supporting session types is an important future work. One possible way of supporting session types would be adopting types of the form `[S]τ` where `S` is a session type by using theories like \[Orchard & Yoshida 2016\].


### Module system

One of the largest features developed these days is the support for a subset of *F-ing modules* \[Rossberg, Russo & Dreyer 2014\], where kinds and functors are restricted to first-order (i.e., type constructors cannot take type constructors as arguments and functors cannot take functors as arguments). For example, Sesterl can type-check the following definition of modules and functors:

```
/* mod.sest */

module Mod = struct

  type option<$a> =
    | None
    | Some($a)

  signature Ord = sig
    type s :: 0
    val compare : fun(s, s) -> int
  end

  module Map = fun(Elem : Ord) ->
    struct
      type elem = Elem.s
      type t<$a> = list<(elem, $a)>
      letrec find<$b>(x : elem, assoc : t<$b>) : option<$b> =
        case assoc of
        | [] ->
            None

        | (k, v) :: tail ->
            if Elem.compare(k, x) == 0 then
              Some(v)
            else
              find(x, tail)
        end
    end

  module Int = struct
    type s = int
    let compare(x : int, y : int) = y - x
  end

  module IntMap = Map(Int)

end
```

The program above is compiled to the following Erlang modules (where line breaks and indentation are manually added for clarity):

```erlang
-module(mod_int).
-export([compare/2]).

compare(S13X, S14Y) -> (S14Y - S13X).
```

```erlang
-module(mod_int_map).
-export([find/2]).

find(S17X, S18Assoc) ->
  case S18Assoc of
    [] ->
      'none';

    [{S19K, S20V} | S21Tail] ->
      case (mod_int:compare(S19K, S17X) == 0) of
        true  -> {'some', S20V};
        false -> mod_int_map:find(S17X, S21Tail)
      end
  end.
```

Note that nested modules are flattened and given lowercased names. This conforms to the naming convention of modules in Erlang.

What is more important here is that functors are eliminated *at compilation time*. This is realized by the technique of so-called the *static interpretation* \[Elsman, Henriksen, Annenkov & Oancea 2018\].


### FFI

Functions written in Erlang can be called from Sesterl via FFI (foreign function interface) as follows:

````
module Ffi = struct

  type option<$a> =
    | None
    | Some($a)

  let assoc<$a> : fun(int, list<(int, $a)>) -> option<($a, list<(int, $a)>)>
    = external 2
  ```
    assoc(Key, Xs) ->
        case lists:keytake(Key, 1, Xs) of
            false                 -> none;
            {value, {_, V}, Rest} -> {some, {V, Rest}}
        end.
  ```

  let main() =
    assoc(1, [
      (3, "Komaba"),
      (1, "Hongo"),
      (4, "Yayoi"),
      (1, "Asano"),
      (5, "Kashiwa")
    ])

end
````

This program compiles to the following implementation:

```erlang
-module(ffi).
-export([assoc/2, main/0]).

assoc(Key, Xs) ->
  case lists:keytake(Key, 1, Xs) of
    false                 -> none;
    {value, {_, V}, Rest} -> {some, {V, Rest}}
  end.

main() ->
  ffi:assoc(1, [
    {3, <<"Komaba">>},
    {1, <<"Hongo">>},
    {4, <<"Yayoi">>},
    {1, <<"Asano">>},
    {5, <<"Kashiwa">>}]).
```


## Future Work

* Support record expressions.
  - Here, records will probably be made polymorphic based on the same type system as SML\# has \[Ohori 1995\].
  - Records in Sesterl does NOT need to compile to Erlang’s record. If needed, you can write FFI that associates records in Sesterl and ones in Erlang.
* Support GADTs.
  - This is mainly for typing `gen_server` callbacks as to synchronous messages.
  - The formalization of such a type system and a type inference algorithm will probably be based on *choice types* \[Chen & Erwig 2016\].
* Support (multiparty) session types.
  - Type checking based on session types may well be optional or something like gradual types. This is because message passing is quite frequent in typical uses of Erlang-style concurrency and thereby strict assertion for sessions may rather complicate in the short term how to program concurrent computations.


### TODO list

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
* [x] FFI
* [ ] Data types
  * [x] Strings/Binaries
  * [x] Product types
  * [x] Lists
  * [x] User-defined ADTs
  * [x] Pattern matching
  * [x] Type synonyms
  * [ ] Records
  * [ ] Functions with labeled parameters
  * [ ] GADTs (especially for typing synchronous messages)
* [x] Mutual recursion by generalized `letrec`-expressions
* [x] Pattern matching by generalized `let`-expressions
* [x] Module system
  * [x] Support for F-ing modules
  * [x] Compilation using the static interpretation
* [ ] Configuration
  * [x] Loading external modules by `require`
  * [ ] Embedding external modules as submodules
* [ ] (Multiparty) session types


## References

* Sheng Chen and Matin Erwig. [Principal type inference for GADTs](https://doi.org/10.1145/2837614.2837665). In *Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL ’16)*, pp. 416–428, 2016.
* Martin Elsman, Troels Henriksen, Danil Annenkov, and Cosmin E. Oancea. [Static interpretation of higher-order modules in Futhark: functional GPU programming in the large](https://dl.acm.org/doi/10.1145/3236792). *Proceedings of the ACM on Programming Languages* 2, ICFP, Article 97, 2018.
* Simon Fowler. [*Typed Concurrent Functional Programming with Channels, Actors, and Sessions*](https://era.ed.ac.uk/handle/1842/35873). PhD thesis, University of Edinburgh, 2019.
* Roger Hindley. The principal type-scheme of an object in combinatory logic. *Transactions of the American Mathematical Society*, **146**, pp. 29–60, 1969.
* Robin Milner. A theory of type polymorphism in programming. *Journal of Computer and System Sciences*, **17**, pp. 348–375, 1978.
* Atsushi Ohori. [A polymorphic record calculus and its compilation](https://dl.acm.org/doi/10.1145/218570.218572). *ACM Transactions on Programming Languages and Systems*, **17**(6), pp. 844–895, 1995.
* Dominic Orchard and Nobuko Yoshida. [Effects as sessions, sessions as effects](https://dl.acm.org/doi/10.1145/2837614.2837634). In *Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL ’16)*, pp. 568–581, 2016.
* Andreas Rossberg, Claudio Russo, and Derek Dreyer. [F-ing modules](https://people.mpi-sws.org/~rossberg/f-ing/). *Journal of Functional Programming*, **24**(5), pp. 529–607, 2014.

# Sesterl: A Session-Typed Erlang

## Summary

*Sesterl* (pronounced as /səsˈtɚːl/) is an ML-like statically-typed functional language that is intended to compile to Erlang. Contrary to its name, Sesterl has not supported session types yet; it only checks the type of messages every process can receive. As mentioned in the section “[Features](#features)” below, however, many features as a typed functional language have already been furnished. Among them are the following:

* First-class higher-order functions
* ADTs and pattern matching
* The standard *Damas–Milner polymorphism* (i.e. so-called the *let-polymorphism*) and *Hindley–Milner type inference* \[Hindley 1969\]\[Milner 1978\]
* Type-level distinction between pure calculations and concurrent computations by a kind of monads \[Fowler 2019\]
* A module system equipped with functors based on *F-ing modules* \[Rossberg, Russo & Dreyer 2014\]


## Table of contents

- [How to install](#how-to-install)
- [How to build source files for development](#how-to-build-source-files-for-development)
- [Example code](#example-code)
- [Features](#features)
  - [Function definition](#function-definition)
  - [Polymorphism](#polymorphism)
  - [ADTs](#adts)
  - [Pattern matching](#pattern-matching)
  - [Concurrency](#concurrency)
  - [Module system](#module-system)
  - [OTP as functors](#otp-as-functors)
  - [FFI](#ffi)
  - [Labeled optional parameters](#labeled-optional-parameters)
  - [Labeled mandatory parameters](#labeled-mandatory-parameters)
  - [Records](#records)
- [Major differences from similar projects](#major-differences-from-similar-projects)
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


## Example code

Example usages can be seen in the following directories:

* [`test/pass/`](https://github.com/gfngfn/Sesterl/tree/master/test/pass)
* [`lib/`](https://github.com/gfngfn/Sesterl/tree/master/lib)


## Features

Sesterl provides many ML-inspired features (i.e. basically resembles OCaml, Standard ML, F\#, Reason, etc.).


### Function definition

Function definition is performed by `let`-expressions:

```
let add(x, y) = x + y
```

Unlike ML family, however, in order to realize seemless compilation to top-level function definitions in Erlang, functions have their own arity (i.e. not curried by nature) and thereby have types of the form `fun(τ_1, …, τ_n) -> τ`. The function `add` defined above, for instance, has type `fun(int, int) -> int`, which is **NOT** equivalent to `fun(int) -> (fun(int) -> int)`.

Incidentally, you do not have to annotate types of arguments or return values; they will be reconstructed by standard *Hindley–Milner type inference*. you can nonetheless add type annotations like the following:

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


### OTP as functors

One of the interesting use cases of the module system is to represent OTP libraries by using functors; for example, `gen_server` can be represented by a functor that takes the callback functions (such as `init/1` or `handle_cast/3`) and related types and that returns functions provided by `gen_server` (such as `cast/2`, `call/3`, `start_link/1`, etc.). The functor `GenServer.Make` defined as follows represents principal functionalities of `gen_server`:

```
module GenServer : sig
  signature Behaviour = sig
    type init_arg :: o
    type request :: o
    type response :: o
    type cast_message :: o
    type state :: o
    val init : fun(init_arg) -> state
    val handle_call<$a> : fun(request, pid<$a>, state) -> (response, state)
    val handle_cast : fun(cast_message, state) -> state
  end

  module Make : fun(Callback : Behaviour) -> sig
    type proc :: o
    val call<$a> : fun(proc, Callback.request, ?timeout int) -> [$a]Callback.response
    val cast<$a> : fun(proc, Callback.cast_message) -> [$a]unit
    val start_link<$a> : fun(Callback.init_arg) -> [$a]proc
  end
end
```


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


### Labeled optional parameters

Functions can have labeled optional parameters:

```
let succ(n : int, ?diff dopt : option<int>) =
  case dopt of
  | None    -> n + 1
  | Some(d) -> n + d
  end

let f(g) =
  (g(36), g(36, ?diff 64))

let main() =
  (succ(42), succ(42, ?diff 15), f(succ))
    /* This evaluates to {43, 57, {37, 100}} in Erlang. */
```

In this example, `?diff` is a label for an optional parameter. By not giving a `?diff`-labeled argument you can use `succ` as the standard successor function, while by giving one you can use `succ` as the integer addition function.

The functions `succ` and `f` defined above are given types as follows:

```
val succ : fun(int, ?diff int) -> int
val f<$a, ?$r :: (?diff int)> : fun(fun(int, ?$r) -> $a) -> ($a, $a)
```

Here, `?diff int` signifies that `succ` can take a `?diff`-labeled optional argument of type `int`, and the absense of other labels in the same domain means that `succ` cannot take optional arguments with labels other than `?diff`.

`?$r :: (?diff int)` is a *row variable* with its kind; it tracks constraints about the minimum set of optional labels that must be able to be given. This is based on an original type system that resembles SML\#’s one for record polymorphism \[Ohori 1995\] (The type system is currently not documented anywhere).


### Labeled mandatory parameters

You can also use labeled mandatory parameters/arguments:

```
letrec foldl(-f f, -init init, -list xs) =
  case xs of
  | []      -> init
  | y :: ys -> foldl(-init f(init, y), -list ys, -f f)
  end
```

Here, `-f`, `-init`, and `-list` are labels for mandatory parameters. Note the order in which labeled arguments are applied is not necessarily the same as that in which labeled parameters are defined. The function `foldl` defined above is typed as follows:

```
val fold<$a, $b> :
  fun(
    -f    fun($a, $b) -> $a,
    -init $a,
    -list list<$b>,
  ) -> $a
```

You can use non-labeled parameters (resp. arguments) and labeled ones for the same function. At least currently, however, their order must be:

1. (possibly empty) non-labeled parameters (resp. arguments),
2. (possibly empty) labeled mandatory ones, and
3. (possibly empty) labeled optional ones.

In other words, abstractions (resp. applications) must be of the following form:

```
fun(param1, …, paramL, -m1 mparam1, … -mM mparamM, ?o1 oparam1, … ?oN oparamN) -> …

f(arg1, …, argL, -m1 marg1, … -mM margM, ?o1 oarg1, … ?oN oargN)
```


## Records

A *record* is a labeled tuple that has the following syntax:

```
{foo = 42, bar = true}
```

Labels should be distinct from each other in one record value. The expression above has the following type:

```
{foo : int, bar : bool}
```

You can also extract values from records as follows:

```
let r = {foo = 42, bar = true} in
r.foo  /* => 42 */
```

In Sesterl, operations for records are made polymorphic by using the same type system as *SML\#* \[Ohori 1995\]. For example, consider the function definition below:

```
let get_foo(x) = x.foo
```

The function `get_foo` is typed like the following:

```
val get_foo<$a, $b :: {foo : $a}> : fun($b) -> $a
```

Here, `{foo : $a}` is a *kind* (i.e. “type of types”) for record types that contain at least `foo : int`. Thanks to the constraint expressed by the kind, `$b` can be instantiated by `{foo : int, bar : bool}`, `{foo : int, baz : binary}`, and so on, but not by `{bar : bool}` etc.  Then, for instance, the following program is well-typed:

```
let main() =
  get_foo({foo = 42, bar = true})
```

and the following is ill-typed on the other hand:

```
let main() =
  get_foo({bar = true})
```


## Major differences from similar projects

There have been brilliant functional languages that compile to Erlang or BEAM (i.e. bytecode for Erlang VM). Some of them are the following:

* [*Elixir*](https://elixir-lang.org/) \[Valim et. al. 2011–2020\]
  - Definitely the most well-known AltErlang language, and well-used in productions.
  - Compiles to BEAM directly.
  - Untyped (i.e. dynamically typed).
  - Has Ruby-like syntax.
  - Supports Lisp-like meta-programming features by quoting/unquoting.
* [*Alpaca*](https://github.com/alpaca-lang/alpaca) \[Pierre et. al. 2016–2019\]
  - Statically typed.
  - Has static guarantee about types of messages sent or received between processes.
  - Has OCaml- or Elm-like syntax.
  - Implemented in Erlang.
* [*Gleam*](https://github.com/gleam-lang/gleam) \[Pilfold et. al. 2019–2020\]
  - Statically typed.
  - Compiles to sources in Erlang.
  - Has Rust-like syntax.
  - Implemented in Rust.

Major differences between the features of Sesterl and those of the languages above are:

* an ML-like module system that supports:
  - abstraction by using signatures, and
  - functors and their elimination at compilation time (called the *static interpretation* \[Elsman, Henriksen, Annenkov & Oancea 2018\]);
* a kind of monadic types for distinguishing pure calculations from concurrent computations.

On the other hand, the following are currently possible weak points:

* No seamless connection with build systems such as [*rebar3*]((https://github.com/erlang/rebar3)).

Also, though not supporting them currently, we want to add features like the following (see “[Future work](#future-work)” for detail):

* GADTs for typing synchronous message-passing operations more strictly.
* Session types in a gradually-typed manner.


## Future Work

* Support GADTs.
  - This is mainly for typing `gen_server` callbacks as to synchronous messages.
  - The formalization of such a type system and a type inference algorithm will probably be based on *choice types* \[Chen & Erwig 2016\].
* Support (multiparty) session types.
  - Type checking based on session types may well be optional or something like gradual types. This is because message passing is quite frequent in typical uses of Erlang-style concurrency and thereby strict assertion for sessions may rather complicate in the short term how to program concurrent computations.
* Connection with [rebar3](https://github.com/erlang/rebar3).


### TODO list

* [ ] Message-passing primitives
  * [x] `spawn`
  * [x] `receive`-expressions
  * [x] `send`
  * [x] `self`
  * [ ] `monitor<$a, $b> : fun(pid<$b>) -> [$a]mref<$b>`
  * [ ] `demonitor<$a, $b> : fun(mref<$b>) -> [$a]unit`
  * [ ] Special message form `down(pid<$a>, mref<$a>, down_info)` representing `{'DOWN', process, Pid, Mref, Info}`
  * [ ] `link<$a, $b> : fun(pid<$b>) -> [$a]unit`
  * [ ] `unlink<$a, $b> : fun(pid<$b>) -> [$a]unit`
* [x] Principal type inference
* [x] Type annotation
* [x] Output Erlang code
* [x] FFI
* [ ] Data types
  * [x] Strings (as lists of code points)
  * [x] Binaries
  * [ ] Durations (e.g. `5000ms`)
  * [ ] Monitoring references `mref<$a>`
  * [ ] Unique references
  * [x] Product types
  * [x] Lists
  * [x] User-defined ADTs
  * [x] Type synonyms
  * [x] Records
  * [x] Functions with labeled optional parameters
  * [x] Functions with labeled mandatory parameters
  * [ ] GADTs (especially for typing synchronous messages)
* [x] Mutual recursion by generalized `letrec`-expressions
* [ ] Pattern matching
  * [x] `case`-expressions
  * [x] Generalized `let`-expressions
  * [ ] Exhaustiveness check
* [ ] Module system
  * [x] Support for F-ing modules
  * [x] Compilation using the static interpretation
  * [ ] First-class modules
* [ ] Configuration
  * [x] Loading external modules by `require`
  * [x] Package system
  * [x] Embedding external modules as submodules
  * [ ] Connection with rebar3
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

# Sesterl: A Session-Typed Erlang

## Summary

*Sesterl* (pronounced as /səsˈtɚːl/) is an ML-like statically-typed functional language that is intended to compile to Erlang. Contrary to its name, Sesterl has not supported session types yet; it only checks the type of messages every process can receive. As mentioned in the section “[Features](#features)” below, however, many features as a typed functional language have already been furnished. Among them are the following:

* First-class higher-order functions
* ADTs and pattern matching
* The standard *Damas–Milner polymorphism* (i.e. so-called the *let-polymorphism*) and *Hindley–Milner type inference* \[Hindley 1969\]\[Milner 1978\]
* Type-level distinction between pure calculations and concurrent computations by a kind of monads \[Fowler 2019\]
* A module system equipped with functors and first-class modules based on *F-ing modules* \[Rossberg, Russo & Dreyer 2014\]


## Table of contents

- [How to install](#how-to-install)
- [How to build source files for development](#how-to-build-source-files-for-development)
- [How to use](#how-to-use)
- [Example code](#example-code)
- [Libraries](#libraries)
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
  - [Doc comments](#doc-comments)
  - [Writing tests](#writing-tests)
- [Major differences from similar projects](#major-differences-from-similar-projects)
- [Future work](#future-work)
  - [TODO list](#todo-list)
- [Configuration file format](#configuration-file-format)
- [Overall syntax](#overall-syntax)
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
$ opam install . --deps-only --with-test
$ make
```


## How to use

### Building a single source file

Invoke:

```console
$ sesterl build <source-file> -o <output-dir>
```

where `<source-file>` is the path to the source file you want to build (e.g. `trial/hello_world.sest`), and `<output-dir>` is the directory where Erlang source files will be generated (e.g. `trial/_generated`).


### Building with Rebar3

[*Rebar3*](https://github.com/erlang/rebar3) is a popular build system for Erlang programs. Sesterl can collaborate with Rebar3.

Based on a configuration file (i.e., `sesterl.yaml`), the following command will generate `rebar.config`:

```console
$ sesterl config ./
```

Then you can invoke the following command to compile Sesterl programs before Rebar3 compiles Erlang code:

```console
$ rebar3 sesterl compile
```

Here, `sesterl` is a name space of Rebar3 commands for compiling Sesterl programs, and is introduced by plugin [`rebar_sesterl`](https://github.com/gfngfn/rebar_sesterl_plugin).

Running unit tests (by using [*EUnit*](http://erlang.org/doc/apps/eunit/chapter.html)) can be done by the following:

```console
$ rebar3 sesterl test
```


## Example code

Example usages can be seen in the following:

* [`examples/` in this repository](https://github.com/gfngfn/Sesterl/tree/master/examples)
* [`test/pass/` in this repository](https://github.com/gfngfn/Sesterl/tree/master/test/pass)
* [`game_tianjiupai`](https://github.com/gfngfn/game_tianjiupai)


## Libraries

* [`sesterl_stdlib`](https://github.com/gfngfn/sesterl_stdlib)
  - The standard library for Sesterl.
  - Contains modules for manipulating basic values and collections (e.g. `Binary`, `List`).
  - Contains modules for constructing OTP-compliant processes (e.g. `GenServer`, `Supervisor`).
* [`sesterl_testing`](https://github.com/gfngfn/sesterl_testing)
  - A testing library for Sesterl.
  - Uses [*EUnit*](http://erlang.org/doc/apps/eunit/chapter.html).
  - Tests written by this module can be run by `rebar3 sesterl test`.
* [`sesterl_json`](https://github.com/gfngfn/sesterl_json)
  - A JSON-handling library.
  - Has APIs similar to those of Elm’s [`elm/json`](https://package.elm-lang.org/packages/elm/json/latest/).
  - Uses [*jsone*](https://github.com/sile/jsone) internally.
* [`sesterl_cowboy`](https://github.com/gfngfn/sesterl_cowboy)
  - A small wrapper for [*Cowboy*](https://github.com/ninenines/cowboy).


## Features

Sesterl provides many ML-inspired features (i.e. basically resembles OCaml, Standard ML, F\#, ReScript, etc.).


### Function definition

Top-level (resp. local) functions are defined by `val`-bindings (resp. `let`-expressions):

```
val add(x, y) = x + y

val add_partial(x) =
  let f(y) = x + y in f
```

Unlike ML family, however, in order to realize seemless compilation to top-level function definitions in Erlang, functions have their own arity (i.e. not curried by nature) and thereby have types of the form `fun(τ_1, …, τ_n) -> τ`. The function `add` defined above, for instance, has type `fun(int, int) -> int`, which is **NOT** equivalent to the type of `add_partial`, i.e., `fun(int) -> (fun(int) -> int)`.

By using `fun`-expressions (i.e. *lambda abstractions*), `add_partial` can also be defined as follows:

```
val add_partial(x) =
  fun(y) -> x + y end
```

Incidentally, you do not have to annotate types of arguments or return values; they will be reconstructed by standard *Hindley–Milner type inference*. you can nonetheless add type annotations like the following:

```
val add(x : int, y : int) : int = x + y
```

You can define higher-order functions, of course:

```
val apply(f, x) = f(x)
```

As is the case in ML, `apply` has a polymorphic type. Features related to type polymorphism is explained later.

Recursive or mutually recursive functions can be defined by using `rec`/`and` keywords, not only globally but also in a local scope:

```
val rec fact(n) =
  if n <= 0 then 1 else n * fact(n - 1)

val is_even_nat(n) =
  let rec odd(n) =
    if n == 0 then false else even(n - 1)

  and even(n) =
    if n == 0 then true else odd(n - 1)
  in
  if n < 0 then false else even(n)
```

Note that, unlike Erlang, function names are all lowercased regardless of whether they are defined in the global scope or in a local one. You can also write, for example, `apply(fact, 6)`; each name of globally-defined functions can be used for the function value the name is bound to, just as locally defined function names can be. This is different from the situation in Erlang, where a globally-defined function name by itself will be interpreted as an atom of the same text.


### Polymorphism

Values defined by `val`, `val rec`, `let`, or `let rec` can be polymorphic. For instance, the function `proj1` defined as follows has type `<$a, $b> fun($a, $b) -> $a` (where `<$a, $b>` stands for universal quantification):

```
val proj1(x, y) = x
```

Instead of relying upon type inference, you can also annotate polymorphic types and check that the defined function is indeed polymorphic:

```
val proj1<$a, $b>(x : $a, y : $b) : $a = x
```


### ADTs

You can define (non-generalized) algebraic data types and type synonyms in a standard way like the following:

```
type name = binary

type with_number<$a> = {$a, int}

type bintree<$b> =
  | Node($b, bintree<$b>, bintree<$b>)
  | Empty
```

Here, `{$a, int}` is an example use of standard product types.

As can be seen from the example above, type names start with a lowercase letter, constructors do with an uppercase one, and type variables are denoted by using a preceding `$`.

Each application of a constructor `Ctor(e_1, …, e_n)` will be compiled to a tuple `{ctor, e_1, …, e_n}` in Erlang where `ctor` is basically a lowercased atom corresponding to `Ctor`. You can, however, change what atoms are generated for constructors by using `#[atom(...)]` attribute:

```
type bintree<$b>
  | #[atom("branch")] Node($b, bintree<$b>, bintree<$b>)
  | #[atom("leaf")]   Empty
```

List-generating constructors, `[]` (nil) and `::` (cons), are also supported by default. Optionals are also provided by default as follows:

```
type option<$a> =
  | #[atom("error")] None
  | #[atom("ok")]    Some($a)
```


### Pattern matching

You can decompose values of ADTs by using `case`-expressions in an ordinary way like the following:

```
val reverse<$a>(xs : list<$a>) : list<$a> =
  let rec aux(acc, xs) =
    case xs of
    | []        -> acc
    | x :: tail -> aux(x :: acc, tail)
    end
  in
  aux([], xs)

val rec tree_size<$a>(t : bintree<$a>) =
  case t of
  | Empty           -> 0
  | Node(_, t1, t2) -> 1 + tree_size(t1) + tree_size(t2)
  end
```


### Concurrency

As in Erlang, you can use primitives `self`, `send`, and `spawn` for message-passing concurrency. They are given types by using a kind of monadic function types `fun(τ_1, …, τ_n) -> [τ]τ'` and types `pid<τ>` for PIDs (i.e. process identifiers) as follows:

* `self<$p> : fun() -> [$p]pid<$p>`
* `send<$p, $q> : fun(pid<$q>, $q) -> [$p]unit`
* `spawn<$p, $q> : fun(fun() -> [$q]unit) -> [$p]pid<$q>`

Intuitively, `[τ]τ'` in `fun(τ_1, …, τ_n) -> [τ]τ'` stands for concurrent computations that will be run on processes capable of receiving messages of type `τ` and that finally produce a value of type `τ'`. The composition of such computations can be done by `do`-notation. Messages can be received by using `receive`-expressions. See a small example below:

```
module Example = struct

  /* dummy */
  val some_heavy_calculation(n) =
    n

  val rec wait_all(msgacc, n) = act
    if n <= 0 then
      return(msgacc)
    else
      receive
      | {pid, msg} ->
          let _ = print_debug(format(f'message ~p received from: ~p~n', {msg, pid})) in
          wait_all(msg :: msgacc, n - 1)
      end

  val rec spawn_all(pidacc, n) = act
    if n <= 0 then
      return(pidacc)
    else
      do parent <- self() in
      do pid <-
        spawn(fun() -> act
          do me <- self() in
          let msg = some_heavy_calculation(n) in
          send(parent, {me, msg})
        end)
      in
      spawn_all(pid :: pidacc, n - 1)

  val main(arg) = act
    let n = 10 in
    do pids <- spawn_all([], n) in
    let _ = print_debug(format(f'spawned: ~p~n', {pids})) in
    do msgs <- wait_all([], n) in
    let _ = print_debug(msgs) in
    return({})

end
```

Here, the primitive `return<$p, $a> : fun($a) -> [$p]$a` lifts a pure value to the computation that has no effect and simply returns the value.

The function `spawn_all` takes an integer `n`, spawns `n` processes that perform some heavy calculation in parallel, and returns their PIDs. `wait_all`, on the other hand, waits all the messages sent from the processes spawned by `spawn_all` and makes a list of the received messages. These functions are typed as follows, supposing `some_heavy_calculation` is of type `fun(int) -> answer`:

* `spawn_all<$p, $q> : fun(list<pid<$q>>, int) -> [$p]list<pid<$q>>`
* `wait_all<$q> : fun(list<answer>, list<pid<$q>>) -> [{pid<$q>, answer}]list<answer>`

As mentioned earlier, supporting session types is an important future work. One possible way of supporting session types would be adopting types of the form `[S]τ` where `S` is a session type by using theories like \[Orchard & Yoshida 2016\].


### Module system

One of the Sesterl’s largest features is the support for a subset of *F-ing modules* \[Rossberg, Russo & Dreyer 2014\], where kinds and functors are restricted to first-order (i.e., type constructors cannot take type constructors as arguments and functors cannot take functors as arguments). For example, Sesterl can type-check the following definition of modules and functors:

```
/* mod.sest */

module Mod = struct

  signature Ord = sig
    type s :: o
    val compare : fun(s, s) -> int
  end

  module Map = fun(Elem : Ord) ->
    struct
      type elem = Elem.s
      type t<$a> = list<{elem, $a}>
      val rec find<$b>(x : elem, assoc : t<$b>) : option<$b> =
        case assoc of
        | [] ->
            None

        | {k, v} :: tail ->
            if Elem.compare(k, x) == 0 then
              Some(v)
            else
              find(x, tail)
        end
    end

  module Int = struct
    type s = int
    val compare(x : int, y : int) = y - x
  end

  module IntMap = Map(Int)

end
```

The program above is compiled to the following Erlang modules (where line breaks and indentation are manually added for clarity):

```erlang
-module('Mod.Int').
-export([compare/2]).

compare(S13X, S14Y) -> (S14Y - S13X).
```

```erlang
-module('Mod.IntMap').
-export([find/2]).

find(S17X, S18Assoc) ->
  case S18Assoc of
    [] ->
      error;

    [{S19K, S20V} | S21Tail] ->
      case ('Mod.Int':compare(S19K, S17X) == 0) of
        true  -> {ok, S20V};
        false -> 'Mod.IntMap':find(S17X, S21Tail)
      end
  end.
```

Note that nested modules are flattened and given names of the form `'<M_1>.<M_2>. ... .<M_n>'` where each `<M_i>` is a module identifier.

What is more important here is that functors are eliminated *at compilation time*. This is realized by the technique of so-called the *static interpretation* \[Elsman, Henriksen, Annenkov & Oancea 2018\].


### OTP as functors

One of the interesting use cases of the module system is to represent OTP libraries by using functors; for example, `gen_server` can be represented by a functor that takes the callback functions (such as `init/1` or `handle_cast/3`) and related types and that returns modules that contains the specialized version of functions provided by `gen_server` (such as `cast/2`, `call/3`, `start_link/1`, etc.). The functor `GenServer.Make` defined in `sesterl_stdlib` as follows represents principal functionalities of `gen_server`:

```
module GenServer : sig

  type initialized :: (o) -> o
  val init_ok<$msg, $state> : fun($state) -> [$msg]initialized<$state>
  val init_stop<$msg, $state> : fun(StopReason.t) -> [$msg]initialized<$state>
  type reply :: (o, o, o) -> o
  val reply<$msg, $response, $state> :
    fun($response, $state, ?timeout int) -> [$msg]reply<$msg, $response, $state>
  val reply_and_stop<$msg, $response, $state> :
    fun(StopReason.t, $response, $state) -> [$msg]reply<$msg, $response, $state>
  type no_reply :: (o) -> o
  val no_reply<$msg, $state> : fun($state, ?timeout int) -> [$msg]no_reply<$state>
  val no_reply_and_stop<$msg, $state> : fun(StopReason.t, $state) -> [$msg]no_reply<$state>
  type start_link_error = RawValue.t
  type call_error = RawValue.t

  signature Behaviour = sig
    type init_arg :: o
    type request :: o
    type response :: o
    type cast_message :: o
    type info :: o
    type state :: o
    type global :: o
    val init : fun(init_arg) -> [info]initialized<state>
    val handle_call<$a> : fun(request, pid<$a>, state) -> [info]reply<info, response, state>
    val handle_cast : fun(cast_message, state) -> [info]no_reply<state>
    val handle_info : fun(info, state) -> [info]no_reply<state>
    val handle_timeout : fun(state) -> [info]no_reply<state>
    val handle_down<$a> : fun(MonitorRef.t, pid<$a>, StopReason.t, state) -> [info]no_reply<state>
    val terminate : fun(StopReason.t, state) -> [info]unit
  end

  module Make : fun(Callback : Behaviour) -> sig
    type proc :: o
    val as_pid : fun(proc) -> pid<Callback.info>
    val from_pid : fun(pid<Callback.info>) -> proc
    val call<$a> : fun(proc, Callback.request, ?timeout int) -> [$a]result<Callback.response, call_error>
    val cast<$a> : fun(proc, Callback.cast_message) -> [$a]unit
    val send_info<$a> : fun(proc, Callback.info) -> [$a]unit
    val start_link<$a> : fun(Callback.init_arg) -> [$a]result<proc, start_link_error>
    val start_link_name<$a> : fun(Callback.init_arg, -name name<Callback.global>) -> [$a]result<proc, start_link_error>
    val where_is_local<$a> : fun(binary) -> [$a]option<proc>
    val where_is_global<$a> : fun(Callback.global) -> [$a]option<proc>
    val stop<$a> : fun(proc) -> [$a]unit
  end
end
```


### FFI

Functions written in Erlang can be called from Sesterl via FFI (foreign function interface) as follows:

````
module FfiExample = struct

  val assoc<$a> : fun(int, list<(int, $a)>) -> option<($a, list<(int, $a)>)> = external 2 ```
assoc(Key, Xs) ->
    case lists:keytake(Key, 1, Xs) of
        false                 -> error;
        {value, {_, V}, Rest} -> {ok, {V, Rest}}
    end.
  ```

  val main() =
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
-module('FfiExample').
-export([assoc/2, main/0]).

assoc(Key, Xs) ->
  case lists:keytake(Key, 1, Xs) of
    false                 -> error;
    {value, {_, V}, Rest} -> {ok, {V, Rest}}
  end.

main() ->
  'FfiExample':assoc(1, [
    {3, <<"Komaba">>},
    {1, <<"Hongo">>},
    {4, <<"Yayoi">>},
    {1, <<"Asano">>},
    {5, <<"Kashiwa">>}]).
```


### Labeled optional parameters

Functions can have labeled optional parameters:

```
val succ(n : int, ?diff dopt : option<int>) =
  case dopt of
  | None    -> n + 1
  | Some(d) -> n + d
  end

val f(g) =
  {g(36), g(36, ?diff 64)}

val main() =
  {succ(42), succ(42, ?diff 15), f(succ)}
    /* This evaluates to {43, 57, {37, 100}} in Erlang. */
```

In this example, `?diff` is a label for an optional parameter. By not giving a `?diff`-labeled argument you can use `succ` as the standard successor function, while by giving one you can use `succ` as the integer addition function.

The functions `succ` and `f` defined above are given types as follows:

```
val succ : fun(int, ?diff int) -> int
val f<$a, ?$r :: (diff)> : fun(fun(int, ?diff int, ?$r) -> $a) -> ($a, $a)
```

Here, `?diff int` signifies that `succ` can take a `?diff`-labeled optional argument of type `int`, and the absense of other labels in the same domain means that `succ` cannot take optional arguments with labels other than `?diff`.

`?$r :: (diff)` is a *row variable* with its kind; it can be instantiated with any rows that do NOT contain the label `diff`; kinds for row variables stand for the prohibited set of labels. This is based on an original type system that resembles record polymorphism \[Gaster & Jones 1996\] (The type system is currently not documented anywhere).


### Labeled mandatory parameters

You can also use labeled mandatory parameters/arguments:

```
val rec foldl(-f f, -init init, -list xs) =
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


### Records

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

In Sesterl, operations for records are made polymorphic by using the type system for extensible rows \[Gaster & Jones 1996\]. For example, consider the function definition below:

```
val get_foo(x) = x.foo
```

The function `get_foo` is typed like the following:

```
val get_foo<$a, ?$r :: (foo)> : fun({foo : $a, ?$r}) -> $a
```

Here, `(foo)` is the kind for row variables that does NOT contain the label `foo`, similar to ones used for optional parameters. Thanks to the constraint expressed by the kind, `{foo : $a, ?$r}` can be instantiated by `{foo : int, bar : bool}`, `{foo : int, baz : binary}`, and so on, but not by `{bar : bool}` etc.  Then, for instance, the following program is well-typed:

```
val main() =
  get_foo({foo = 42, bar = true})
```

and the following is ill-typed on the other hand:

```
val main() =
  get_foo({bar = true})
```

Note: Prior to Sesterl 0.2.0, polymorphic typing for records was based on the one used in *SML\#* \[Ohori 1995\].


### Doc comments

You can add doc comments to members in signatures by using `#[doc(String)]` attribute where `String` is an arbitrary string literal containing a text in Markdown:

````
module List :> sig
  ...

  #[doc(```
    `map f [v_1, …, v_n]` applies function `f` to each `v_i` in the given order,
    and builds the list [f v_1, …, f v_n] with the results produced by `f`.
  ```)]
  val map<$a, $b> : fun(fun($a) -> $b, list<$a>) -> list<$b>

  ...
end = struct
  ...
end
````

(Note: The outermost triple back ticks in the example above are NOT included in Markdown contents; they just start/terminate the string literal as double quotes do. If you want to use triple back ticks in Markdown contents to display code blocks, you can use quadruple back ticks for enclosing string literals.)

You can, for example, generate documents `./_docs/your_package.html` by specifying the following description in your configuration file:

```yaml
document_outputs:
  - format:
      type: "html"
    output_directory: "./_doc"
```


## Writing tests

You can write test modules like the following:

```
./
├── README.md
├── sesterl.yaml
├── rebar.config
├── rebar.lock
├── src/
│   └── Adder.sest
└── test/
    └── AdderTests.sest
```

`sesterl.yaml`:

```
package: "adder"
language: "v0.2.0"
source_directories:
  - "./src"
main_module: "Adder"
test_directories:
  - "./test"
```

`src/Adder.sest`:

```
module Adder = struct

  val add(m, n) = m + n

end
```

`test/AdderTests.sest`:

```
import Adder

module AdderTests = #[test] struct

  #[test]
  val adder_test() =
    Testing.it("42 plus 57 equals 99", fun() ->
      assert Testing.equal(
        -expect 99,
        -got    Adder.add(42, 57),
      )
    end)

end
```

The following makes the test run:

```
$ sesterl config ./
$ rebar3 sesterl test
```


## Major differences from similar projects

There have been brilliant functional languages that compile to Erlang or BEAM (i.e. bytecode for Erlang VM). Some of them are the following:

* [*Elixir*](https://elixir-lang.org/) \[Valim et al. 2011–2021\]
  - Definitely the most well-known AltErlang language, and well-used in productions.
  - Compiles to Erlang AST.
  - Untyped (i.e. dynamically typed).
  - Has Ruby-like syntax.
  - Supports Lisp-like meta-programming features by quoting/unquoting.
* [*Alpaca*](https://github.com/alpaca-lang/alpaca) \[Pierre et al. 2016–2019\]
  - Statically typed.
  - Compiles to Core Erlang compiler IR.
  - Has static guarantee about types of messages sent or received between processes.
  - Has OCaml- or Elm-like syntax.
  - Implemented in Erlang.
* [*Gleam*](https://github.com/gleam-lang/gleam) \[Pilfold et al. 2018–2021\]
  - Statically typed.
  - Compiles to sources in Erlang.
  - Has Rust-like syntax.
  - Implemented in Rust.

Major differences between the features of Sesterl and those of the languages above are:

* an ML-like module system that supports:
  - abstraction by using signatures, and
  - functors and their elimination at compilation time (called the *static interpretation* \[Elsman, Henriksen, Annenkov & Oancea 2018\]);
* a kind of monadic types for distinguishing pure calculations from concurrent computations.

Also, though not supporting them currently, we want to add features like the following (see “[Future work](#future-work)” for detail):

* GADTs for typing synchronous message-passing operations more strictly.
* Session types in a gradually-typed manner.


## Future work

* Support recursive modules.
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
  * [x] `MonitorRef.monitor<$a, $b> : fun(pid<$b>) -> [$a]MonitorRef.t`
  * [x] `MonitorRef.demonitor<$a> : fun(MonitorRef.t) -> [$a]unit`
  * [ ] Special message `down(MonitorRef.t, StopReason.t)` representing `{'DOWN', MRef, process, Pid, Reason}`
  * [ ] `link<$a, $b> : fun(pid<$b>) -> [$a]unit`
  * [ ] `unlink<$a, $b> : fun(pid<$b>) -> [$a]unit`
* [x] Principal type inference
* [x] Type annotation
* [x] Output Erlang code
* [x] FFI
* [ ] Data types
  * [x] Strings (as lists of code points)
  * [x] Binaries
  * [x] Monitoring references `MonitorRef.t`
  * [ ] Unique references
  * [x] Product types
  * [x] Lists
  * [x] User-defined ADTs
  * [x] Type synonyms
  * [x] Records
  * [x] Functions with labeled optional parameters
  * [x] Functions with labeled mandatory parameters
  * [ ] GADTs (especially for typing synchronous messages)
* [x] Mutual recursion by generalized `val rec`-expressions
* [ ] Pattern matching
  * [x] `case`-expressions
  * [x] Generalized `let`-expressions
  * [ ] Exhaustiveness check
* [x] Module system
  * [x] Support for F-ing modules
  * [x] Compilation using the static interpretation
  * [x] First-class modules
* [x] Configuration
  * [x] Loading external modules by `import`
  * [x] Package system
  * [x] Embedding external modules as submodules
  * [x] Connection with Rebar3
* [ ] (Multiparty) session types


## Configuration file format

Configuration files must be of the following form. Although configuration files are in the YAML format, their specification is described here by using JSON-like expressions for clarity of the structure:

```
Config := {
  package: String
    # The name of the package. Example: "sesterl_json"

  language: String
    # The minimum version of Sesterl required by the package.
    # Example: "v0.2.0"
    # The Sesterl compiler refers to this field for checking that
    # the compiler is backward-compatible with the required version.
    # This field is optional. No check will be performed if omitted.

  source_directories: Array<String>
    # The list of directories where source files are placed.
    # All the source files (i.e. files that have
    # ".sest", ".erl", or ".app.src" as their extension)
    # that are exactly at one of the specified directories will be used for compilation.
    # Specified directories must be relative to the configuration file.
    # Example: [ "./src", "./src/generated" ]

  main_module: String
    # The name of the main module of the package.
    # The *main module* of a package is defined to be
    # the sole module visible from the outside of the package.

  test_directories: Array<String>
    # The list of directories where test files are placed.
    # Specified directories must be relative to the configuration file.
    # This field is optional. Default: []
    # Example: [ "./test" ]

  dependencies: Array<Dependency>
    # This field is optional. Default: []

  test_dependencies: Array<Dependency>
    # This field is optional. Default: []

  erlang: ErlangConfig
    # This field is optional. Default: {}

  document_outputs: Array<DocumentOutput>
    # Settings for the document generation.
    # This field is optional. Default: []
}

Dependency := {
  name: String
    # The name of the dependency.

  source: (GitSource | LocalSource)
    # Describes how to get the dependency.
}

GitSource := {
  type: "git"

  repository: String
    # The URI of the Git repository.

  spec: (TagSpec | RefSpec | BranchSpec)
    # Describes which commit to use.
}

TagSpec := {
  type: "tag"
  value: String  # Example: "v1.3.0"
}

RefSpec := {
  type: "ref"
  value: String  # A commit hash.
}

BranchSpec := {
  type: "branch"
  value: String  # Example: "master"
}

LocalSource := {
  type: "local"

  directory: String
    # The directory where the dependency is placed.
}

HexSource := {
  type: "hex"

  version: String
    # The version number.
}

ErlangConfig := {
  output_directory: String
    # The directory at which Erlang modules are generated.
    # Must be relative to the configuration file.
    # This field is Optional. Default: "./_generated"

  test_output_directory: String
    # The directory at which Erlang test modules for EUnit are generated.
    # Must be relative to the configuration file.
    # This field is Optional. Default: "./_generated_test"

  erlang_dependencies: Array<ErlangDependency>
    # The Erlang libraries on which the package depends.
    # This field is optional. Default: []

  relx: Relx
    # This field is optional.
    # No `relx` stanza will be written on `rebar.config` if omitted.
}

ErlangDependency := {
  name: String
    # The name of the package. Example: "cowboy"

  source: (HexSource | GitSource)
    # Describes how to get the Erlang library.
}

Relx := {
  release: RelxRelease
  dev_mode: Boolean     # This field is optional. Default: false
}

RelxRelease := {
  name: String
  version: String
  applications: Array<String>
}

DocumentOutput := {
  format: { type: "html" }
    # The format of output documents.
    # Only HTML is supported so far.

  output_directory: String
    # The directory at which documents are generated.
    # Must be relative to the configuration file.
    # Example: [ "./_doc" ]
}
```


## Overall syntax

How to read:

* a word enclosed by single quotation marks (e.g. `'let'` or `'('`):
  - a keyword token or a symbol token
* a word without quotations (e.g. `E` or `val-args`):
  - a metavariable of the (extended) BNF
* `(empty)`
  - the empty sequence of tokens
* `( DESCR )*`
  - a possibly empty finite repetition of `DESCR`
* `( DESCR )+`
  - equals `DESCR ( DESCR )*`
* `(empty)`
  - no token (i.e. a token sequence of length zero)
* `(DESCR1 | DESCR2)`
  - either `DESCR1` or `DESCR2`
* `( DESCR )?`
  - equals `((empty) | DESCR)`

```
n ::= (decimal or hexadecimal integer literals)
float-lit ::= (floating-point number literals)
bin-lit ::= (string literals enclosed by double quotation marks)
X, C ::= (capitalized identifiers)
x, t, k, l ::= (lowercased identifiers other than keywords)
$a ::= (lowercased identifiers preceded by a dollar sign)
?$a ::= (lowercased identifiers preceded by a question mark and a dollar sign)
-l ::= (lowercased identifiers preceded by a hyphen)
?l ::= (lowercased identifiers preceded by a question mark)

# source files:
source-file ::=
  | ('import' X)* 'module' X (':>' S) '=' 'struct' (open-spec)* (bind)* 'end'

# value expressions:
E ::=
  | '(' E ')'
  | E binary-operator E
  | (X '.')* x
  | (X '.')* C                                      # variant constructors
  | E '(' val-args ')'                              # function applications
  | 'let' bind-val-local 'in' E                     # local bindings
  | 'let' pattern '=' E 'in' E                      # local bindings by the pattern matching
  | 'fun' '(' val-params ')' '->' E 'end'           # pure abstractions
  | 'fun' '(' val-params ')' '->' 'act' P 'end'     # effectful abstractions
  | 'if' E 'then' E 'else' E                        # conditionals
  | 'case' E 'of' (pure-case)+ 'end'                # pattern-matching expressions
  | '{' '}'                                         # the unit value
  | '{' E (',' E)* (',')? '}'                       # tuples
  | '{' l '=' E (',' l '=' E)* (',')? '}'           # records
  | E '.' l                                         # record access
  | '{' E '|' l '=' E (',' l '=' E)* (',')? '}'     # record update
  | 'freeze' (X '.')* x '(' freeze-args ')'         # so-called (possibly partial) mfargs() in Erlang
  | 'freeze' '(' E ')' 'with' '(' freeze-args ')'   # addition of arguments to partial mfargs()
  | 'pack' M ':' S                                  # packed first-class modules
  | 'assert' E                                      # assertion for tests
  | E '::' E                                        # cons
  | '[' ']'                                         # nil
  | n
  | float-lit
  | bin-lit
  | 'true'
  | 'false'
  | ...

pure-case ::=
  | '|' pattern '->' E

# effectful computations:
P ::=
  | 'do' pattern '<-' P 'in' P                     # sequential compositions (i.e. so-called monadic binds)
  | 'receive' (effectful-case)+ after-branch 'end' # selective receive
  | E '(' val-args ')'                             # function applications
  | 'if' E 'then' P 'else' P                       # conditionals
  | 'case' E 'of' (effectful-case)+ 'end'          # pattern-matching expressions

effectful-case ::=
  | '|' pattern '->' P

after-branch ::=
  | (empty)
  | 'after' E '->' P

# sequences of arguments for function applications:
val-args ::=
  | E (',' val-args)?
  | val-labeled-args

val-labeled-args ::=
  | -l E (',' val-labeled-args)?
  | val-optional-args

val-optional-args ::=
  | ?l E (',' val-optional-args)?
  | (empty)

# patterns for the pattern matching:
pattern ::=
  | '_'                                     # wildcard
  | x                                       # variable binding
  | C                                       # constructors with no argument
  | C '(' pattern (',' pattern)* (',')? ')' # constructors with arguments
  | '{' '}'                                 # the unit pattern
  | '{' pattern (',' pattern)* (',')? '}'   # tuples
  | pattern '::' pattern                    # cons
  | '[' ']'                                 # nil
  | n
  | bin
  | 'true'
  | 'false'
  | ...

# types:
T ::=
  | $a                                       # type variables
  | (X '.')* t ty-args                       # applications of type constructors
  | 'fun' '(' ty-doms ')' '->' T             # function types
  | 'fun' '(' ty-doms ')' '->' '[' T ']' T   # action types
  | '{' T (',' T)* (',')? '}'                # product types
  | '{' l '=' T (',' l '=' T)* (',')? '}'    # record types
  | 'pack' S                                 # types for first-class modules

# sequences of type arguments:
ty-args ::=
  | ('<' ty-args-sub '>')?

ty-args-sub ::=
  | T (',' ty-args-sub)?
  | (empty)

# sequences of domain types:
ty-doms ::=
  | T (',' ty-doms)?
  | ty-labeled-doms

ty-labeled-doms ::=
  | -l T (',' ty-labeled-doms)?
  | ty-optional-doms
  | ?$a

ty-optinal-doms ::=
  | ?l T (',' ty-optional-doms)?
  | (empty)

# a kind:
K ::=
  | kd-base                                              # base kinds (i.e. order-0 kinds)
  | '(' kd-base (',' kd-base)* (',')? ')' '->' kd-base   # order-1 kinds

kd-base ::=
  | k      # named base kinds (currently only 'o' is provided)
  | kd-row # row kinds

kd-row ::=
  | '(' labels ')'

labels ::=
  | l ',' labels
  | l
  | (empty)

open-spec ::=
  | 'open' (X '.')* X

# module expressions:
M ::=
  | '(' M ')'
  | (X '.')* X
  | 'struct' (open-spec)* (bind)* 'end' # structures
  | 'fun' '(' X ':' S ')' '->' M        # functor abstractions
  | (X '.')* X '( M )'                  # functor applications
  | X ':>' S                            # coercion

# bindings (i.e. members of structures):
bind ::=
  | 'val' (bind-val-local | bind-val-ffi)
  | 'type' bind-ty
  | 'module' X (':>' S)? '=' M
  | 'signature' X '=' S
  | 'include' M

# signature expressions:
S ::=
  | '(' S ')'
  | (X '.')* X
  | 'sig' (open-spec)* (decl)* 'end' # structure signatures
  | 'fun' '(' X ':' S ')' '->' S     # functor signatures
  | S 'with' 'type' bind-ty

# declarations (i.e. members of structure signatures):
decl ::=
  | 'val' x ty-quant ':' T
  | 'type' t ('::' K)?
  | 'type' t '=' bind-ty
  | 'module' X ':' S
  | 'signature' X '=' S

bind-val-local ::=
  | bind-val-single                                  # non-recursive definitions
  | 'rec' bind-val-single ('and' bind-val-single)*   # (mutually) recursive definitions

bind-val-single ::=
  | x ty-quant '(' val-params ')' (':' T)? '=' E                   # function definitions
  | x ty-quant '(' val-params ')' (':' '[' T ']' T)? '=' 'act' P   # action definitions

bind-val-ffi ::=
  | x ty-quant ':' T '=' 'external' n ('+')? string-block  # FFI

bind-ty ::=
  | bind-ty-single ('and' bind-ty-single)*

bind-ty-single ::=
  | t ty-quant '=' ('|')? ctor-branch ('|' ctor-branch)*   # variant type definitions
  | t ty-quant '=' T                                       # type synonym definitions

ctor-branch ::=
  | C ('(' T (',' T)* ')')?   # a definition of a constructor and its parameter types

# comma-separated sequences of value parameters (for function definitions):
val-params ::=
  | pattern (':' T)? (',' val-params)?
  | val-labeled-params

# comma-separated labeled parameters:
val-labeled-params ::=
  | -l pattern (':' T)? (',' val-labeled-params)?
  | val-optional-params

# comma-separated labeled optional parameters (possibly with default expressions):
val-optional-params ::=
  | ?l pattern (':' T)? ('=' E)? (',' val-optional-params)?
  | (empty)

# sequences of universal quantifiers for type parameters and row parameters
ty-quant ::=
  | ('<' ty-params '>')?

ty-params ::=
  | $a ',' ty-params
  | $a
  | row-params

row-params ::=
  | ?$a '::' kd-row (',' row-params)?
  | (empty)
```


## References

* Sheng Chen and Martin Erwig. [Principal type inference for GADTs](https://doi.org/10.1145/2837614.2837665). In *Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL ’16)*, pp. 416–428, 2016.
* Martin Elsman, Troels Henriksen, Danil Annenkov, and Cosmin E. Oancea. [Static interpretation of higher-order modules in Futhark: functional GPU programming in the large](https://dl.acm.org/doi/10.1145/3236792). *Proceedings of the ACM on Programming Languages* 2, ICFP, Article 97, 2018.
* Simon Fowler. [*Typed Concurrent Functional Programming with Channels, Actors, and Sessions*](https://era.ed.ac.uk/handle/1842/35873). PhD thesis, University of Edinburgh, 2019.
* Benedict R. Gaster and Mark P. Jones. [A polymorphic type system for extensible records and variants](https://web.cecs.pdx.edu/~mpj/pubs/96-3.pdf). Technical Report NOTTCS-TR-96-3, 1996.
* Roger Hindley. The principal type-scheme of an object in combinatory logic. *Transactions of the American Mathematical Society*, **146**, pp. 29–60, 1969.
* Robin Milner. A theory of type polymorphism in programming. *Journal of Computer and System Sciences*, **17**, pp. 348–375, 1978.
* Atsushi Ohori. [A polymorphic record calculus and its compilation](https://dl.acm.org/doi/10.1145/218570.218572). *ACM Transactions on Programming Languages and Systems*, **17**(6), pp. 844–895, 1995.
* Dominic Orchard and Nobuko Yoshida. [Effects as sessions, sessions as effects](https://dl.acm.org/doi/10.1145/2837614.2837634). In *Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL ’16)*, pp. 568–581, 2016.
* Andreas Rossberg, Claudio Russo, and Derek Dreyer. [F-ing modules](https://people.mpi-sws.org/~rossberg/f-ing/). *Journal of Functional Programming*, **24**(5), pp. 529–607, 2014.

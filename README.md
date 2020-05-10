# [WIP] Sesterl: A Session-Typed Erlang

*Sesterl* (pronounced as /səsˈtɚːl/) is a statically-typed functional language that is intended to compile to Erlang.

Contrary to its name, Sesterl has not supported session types yet; it only checks the type of messages every process can receive, and has type-level distinction between pure calculations and concurrent computations by a kind of monads \[Fowler 2019\].

As mentioned below, however, many features as a typed functional language have already been furnished.


## Features

Sesterl provides many ML-inspired features (i.e. basically resembles OCaml, Standard ML, Reason, etc.).


### Function definition

Function definition is performed by `let`-expressions:

```
let add(x, y) = x + y
```

Unlike ML family, however, in order to realize seemless compilation to top-level function definitions in Erlang, functions have their own arity (i.e. not curried by nature) and thereby have types of the form `fun(τ_1, …, τ_n) -> τ`. The function `add` defined above, for instance, has type `fun(int, int) -> int`, which is **not** equivalent to `fun(int) -> fun(int) -> int`.

Incidentally, you do not have to annotate types of arguments or return values; they will be reconstructed by standard *Hindley–Milner type inference*. you can nonetheless add type annotation like the following:

```
let add(x: int, y: int): int = x + y
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
    if n <= 0 then even(n - 1)

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
let proj1<$a, $b>(x: $a, y: $b): $a = x
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
let reverse<$a>(xs: list<$a>): list<$a> =
  letrec aux(acc, xs) =
    case xs of
    | []        -> acc
    | x :: tail -> aux(x :: acc, tail)
    end
  in
  aux([], xs)

letrec tree_size(t: bintree<$a>) =
  case t of
  | Empty           -> 0
  | Node(_, t1, t2) -> 1 + tree_size(t1) + tree_size(t2)
  end
```


### Concurrency

As in Erlang, you can use primitives `self`, `send`, and `spawn` for message-passing concurrency. They are given types by using a kind of monadic types `[τ_0]τ_1` and types `pid<τ>` for PIDs (i.e. process identifiers) as follows:

* `self<$p>: [$p]pid<$p>`
* `send<$p, $q>: fun(pid<$q>, $q) -> [$p]unit`
* `spawn<$p, $q>: fun([$q]unit) -> [$p]pid<$q>`

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

Here, the primitive `return<$p, $a>: fun($a) -> [$p]$a` lifts a pure value to the computation that has no effect and simply returns the value.

The function `spawn_all` takes an integer `n`, spawns `n` processes that perform some heavy calculation in parallel, and returns their PIDs. `wait_all`, on the other hand, waits all the messages sent from the processes spawned by `spawn_all` and makes a list from the messages. These functions are typed as follows, supposing `some_heavy_calculation` is of type `fun(int) -> answer`:

* `spawn_all<$p, $q>: fun(list<pid<$q>>, int) -> [$p]list<pid<$q>>`
* `wait_all<$q>: fun(list<answer>, list<pid<$q>>) -> [(pid<$q>, answer)]list<answer>`

As mentioned earlier, supporting session types is an important future work. One possible way of supporting session types would be adopting types of the form `[S]τ` where `S` is a session type by using theories like \[Orchard & Yoshida 2016\].


### Module system

One of the largest features developed these days is the support for a subset of *F-ing modules* \[Rossberg, Russo & Dreyer 2014\], where kinds are restricted to first-order (i.e., type constructors cannot take type constructors as their arguments). For example, Sesterl can type-check the following definition of modules and functors:

```
type option<$a> =
  | None
  | Some($a)

signature Ord = sig
  type s:: 0
  val compare: fun(s, s) -> int
end

module Map = fun(Elem: Ord) ->
  struct
    type elem = Elem.s
    type t<$a> = list<(elem, $a)>
    letrec find<$b>(x: elem, assoc: t<$b>): option<$b> =
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
  let compare(x: int, y: int) = y - x
end

module IntMap = Map(Int)
```

Currently, however, back-end Erlang code generator has not supported modules yet; for now, only type checking can be performed for modules.


## References

* Simon Fowler. *Typed Concurrent Functional Programming with Channels, Actors, and Sessions*. PhD thesis, University of Edinburgh, 2019.
* Dominic Orchard and Nobuko Yoshida. Effects as sessions, sessions as effects. In *Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL’16)*, pp. 568–581, 2016.
* Andreas Rossberg, Claudio Russo, and Derek Dreyer. F-ing modules. *Journal of Functional Programming*, **24**(5), pp. 529–607, 2014.


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
  * [x] Type synonyms
  * [ ] Records
* [x] Mutual recursion by generalized `letrec`-expressions
* [x] Pattern matching by generalized `let`-expressions
* [x] Module system
* [ ] (Multiparty) session types

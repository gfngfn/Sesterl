
open Syntax


let warn_val_not_used (rng : Range.t) (x : identifier) =
  Format.printf "* [Warning] %a: variable '%s' is unused\n"
    Range.pp rng
    x

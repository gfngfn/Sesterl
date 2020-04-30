
type t = string


let make (ctor : string) =
  ctor


let pp ppf ctor =
  Format.fprintf ppf "\"%s\"" ctor


let output ctorid =
  ctorid  (* temporary; should lowercase the first letter *)

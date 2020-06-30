
type t = IdentifierScheme.t


let make : string -> t option =
  IdentifierScheme.from_upper_camel_case


let pp ppf ctorid =
  Format.fprintf ppf "C\"%a\"" IdentifierScheme.pp ctorid


let output (ctorid : t) : string =
  Printf.sprintf "'%s'" (IdentifierScheme.to_snake_case ctorid)

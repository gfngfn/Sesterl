
type t = IdentifierScheme.t


let is_latin_uppercase (ch : char) =
  'A' <= ch && ch <= 'Z'


let make : string -> t option =
  IdentifierScheme.from_upper_camel_case


let pp ppf ctorid =
  Format.fprintf ppf "C\"%a\"" IdentifierScheme.pp ctorid


let output (ctorid : t) : string =
  IdentifierScheme.to_snake_case ctorid


type t = string


let is_latin_uppercase (ch : char) =
  'A' <= ch && ch <= 'Z'


let make (s : string) : t =
  if String.length s <= 0 then
    assert false
  else
    let char = String.get s 0 in
    if is_latin_uppercase char then
      String.make 1 (Char.lowercase_ascii char) ^ (String.sub s 1 (String.length s - 1))
    else
      assert false


let pp ppf ctor =
  Format.fprintf ppf "\"%s\"" ctor


let output (ctorid : t) : string =
  ctorid

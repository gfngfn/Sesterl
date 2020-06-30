(**
    Every fragment should be a non-empty string consisting only of lowercase letters and digits.

    `to_upper_camel_case name` outputs `name` in upper camel case
    (with inserting underscores before every fragment that begins with a digit):

*)

type t = {
  fragments : string list;
  original  : string;
}

let is_digit ch =
  '0' <= ch && ch <= '9'


let is_lowercase ch =
  'a' <= ch && ch <= 'z'

let is_uppercase ch =
  'A' <= ch && ch <= 'Z'

let to_lowercase ch =
  Char.chr (Char.code ch + 0x20)


let string_of_chars (chs : char list) : string =
  let len = List.length chs in
  let buf = Buffer.create len in
  chs |> List.iter (Buffer.add_char buf);
  Buffer.contents buf


let is_valid_fragment s =
  String.length s > 0 && String.equal s (String.lowercase_ascii s)


let is_valid =
  List.for_all is_valid_fragment


let from_snake_case (original : string) : t option =
  let fragments = String.split_on_char '_' original in
  if is_valid fragments then Some({ fragments; original; }) else None


let from_upper_camel_case (original : string) : t option =
  let len = String.length original in
  let rec aux (fragacc : string list) (chacc : char list) (index : int) =
    if index >= len then
      let fragment = string_of_chars (List.rev chacc) in
      let fragments = List.rev (fragment :: fragacc) in
      Some{ fragments; original }
    else
      let ch = String.get original index in
      if is_uppercase ch then
        let fragment = string_of_chars (List.rev chacc) in
        aux (fragment :: fragacc) [ to_lowercase ch ] (index + 1)
      else if is_lowercase ch || is_digit ch then
        aux fragacc (ch :: chacc) (index + 1)
      else if ch = '_' then
        let ch2 = String.get original (index + 1) in
        if is_digit ch2 then
          let fragment = string_of_chars (List.rev chacc) in
          aux (fragment :: fragacc) [ ch ] (index + 2)
        else
          None
      else
        None
  in
  try
    let ch0 = String.get original 0 in
    if is_uppercase ch0 then
      aux [] [ to_lowercase ch0 ] 1
    else
      None
  with
  | Invalid_argument(_) -> None


let original (name : t) : string =
  name.original


let to_snake_case (name : t) : string =
  name.fragments |> String.concat "_"


let capitalize (is_lower_first : bool) (fragment : string) =
  if is_lower_first then
    fragment
  else
    String.capitalize_ascii fragment


let camel_case (is_lower : bool) (name : t) : string =
  let rec aux is_lower_first acc = function
    | [] ->
        List.rev acc

    | x :: [] ->
        List.rev (capitalize is_lower_first x :: acc)

    | x :: ((y :: _) as rest) ->
        let xcap = capitalize is_lower_first x in
        if is_digit (String.get y 0) then
          aux false ((xcap ^ "_") :: acc) rest
        else
          aux false (xcap :: acc) rest
  in
  aux is_lower [] name.fragments |> String.concat ""


let to_lower_camel_case = camel_case true

let to_upper_camel_case = camel_case false


let pp ppf name =
  Format.fprintf ppf "<\"%s\">" (to_snake_case name)


let compare name1 name2 =
  String.compare name1.original name2.original

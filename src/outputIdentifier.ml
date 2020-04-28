
type answer =
  | Normal   of string
  | Operator of string

type t = answer


let is_latin_lowercase char =
  'a' <= char && char <= 'z'


let fresh =
  let current_max = ref 0 in
  (fun () ->
    incr current_max;
    let s = Printf.sprintf "GenSym%d" (!current_max) in
    Normal(s)
  )


let scheme (should_capitalize : bool) (s : string) : t =
  if String.length s <= 0 then
    assert false
  else
    let char = String.get s 0 in
    if is_latin_lowercase char then
      let sret =
        if should_capitalize then
          String.make 1 (Char.uppercase_ascii char) ^ (String.sub s 1 (String.length s - 1))
        else
          s
      in
      Normal(sret)
    else
      fresh ()


let local (s : string) : t =
  scheme true s


let global (s : string) : t =
  scheme false s


let global_operator (s : string) : t =
  Operator(s)


let output (ans : t) : answer =
  ans


let pp ppf (ans : t) =
  match ans with
  | Normal(s) | Operator(s) -> Format.fprintf ppf "\"%s\"" s


let unused =
  Normal("_")

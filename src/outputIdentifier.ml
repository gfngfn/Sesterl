
type answer =
  | Local    of string
  | Global   of string * int
  | Operator of string

type t = answer


let is_latin_lowercase char =
  'a' <= char && char <= 'z'


let fresh =
  let current_max = ref 0 in
  (fun () ->
    incr current_max;
    let s = Printf.sprintf "GenSym%d" (!current_max) in
    Local(s)
  )


let scheme (f : string -> t) (should_capitalize : bool) (s : string) : t =
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
      f sret
    else
      fresh ()


let local (s : string) : t =
  scheme (fun x -> Local(x)) true s


let global (s : string) (arity : int) : t =
  scheme (fun x -> Global(x, arity)) false s


let global_operator (s : string) : t =
  Operator(s)


let output (ans : t) : answer =
  ans


let pp ppf (ans : t) =
  match ans with
  | Local(s) | Operator(s) -> Format.fprintf ppf "\"%s\"" s
  | Global(s, arity)       -> Format.fprintf ppf "\"%s/%d\"" s arity


let unused =
  Local("_")

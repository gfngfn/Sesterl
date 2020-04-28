
type t = string


let local (s : string) : t option =
  if String.length s <= 0 then
    None
  else
    let char = String.get s 0 in
    let s = String.make 1 (Char.uppercase_ascii char) ^ (String.sub s 1 (String.length s - 1)) in
    Some(s)


let global (s : string) : t option =
  Some(s)


let to_string (s : t) : string = s


let pp ppf (s : t) =
  Format.fprintf ppf "\"%s\"" s


let fresh =
  let current_max = ref 0 in
  (fun () ->
    begin
      incr current_max;
      Printf.sprintf "SesterlGenSymbol%d" (!current_max)
    end)


let unused =
  "_"

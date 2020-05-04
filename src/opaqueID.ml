
type t = {
  id   : int;
  name : string;
}


let hash =
  Hashtbl.hash


let compare oid1 oid2 =
  oid2.id - oid1.id



let equal oid1 oid2 =
  oid1.id = oid2.id


let fresh =
  let current_max = ref 0 in
  (fun name ->
    incr current_max;
    { id = !current_max; name = name }
  )


let pp ppf oid =
  Format.fprintf ppf "%s" oid.name

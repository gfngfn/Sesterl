
type t = {
  number : int;
  name   : string;
}

let fresh =
  let current_max = ref 0 in
  (fun name ->
    incr current_max;
    {
      number = !current_max;
      name   = name;
    }
  )


let hash =
  Hashtbl.hash


let compare tyid1 tyid2 =
  tyid2.number - tyid1.number


let equal tyid1 tyid2 =
  tyid1.number = tyid2.number


let pp ppf tyid =
  Format.fprintf ppf "%s" tyid.name

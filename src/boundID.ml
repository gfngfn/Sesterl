
type t = {
  id : int;
}


let equal bid1 bid2 =
  bid1.id = bid2.id


let hash bid =
  bid.id


let compare bid1 bid2 =
  bid2.id - bid1.id


let current_max = ref 0


let initialize () =
  current_max := 0


let fresh ~message () =
  incr current_max;
  { id = !current_max; }


let pp ppf bid =
  Format.fprintf ppf "'#%d" bid.id

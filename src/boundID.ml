
type t = {
  id : int;
}


let equal bid1 bid2 =
  bid1.id = bid2.id


let hash = Hashtbl.hash


let current_max = ref 0


let initialize () =
  current_max := 0


let fresh () =
  incr current_max;
  { id = !current_max; }

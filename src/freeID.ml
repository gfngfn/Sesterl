type level = int

type t =
  { id : int
  ; mutable level : level
  }

let equal fid1 fid2 = fid1.id = fid2.id

let hash = Hashtbl.hash

let current_max = ref 0

let initialize () = current_max := 0

let fresh lev =
  incr current_max;
  { id = !current_max; level = lev }

let get_level fid = fid.level

let update_level fid lev = fid.level <- min fid.level lev

let pp ppf fid = Format.fprintf ppf "'%d" fid.id

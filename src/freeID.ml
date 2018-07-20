
type t = int


let equal fid1 fid2 =
  fid1 = fid2


let current_max = ref 0


let initialize () =
  current_max := 0


let fresh () =
  incr current_max;
  !current_max


let pp ppf fid =
  Format.fprintf ppf "'%d" fid

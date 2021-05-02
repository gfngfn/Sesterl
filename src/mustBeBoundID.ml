
type t = {
  main  : BoundID.t;
  level : int;
}


let fresh (lev : int) =
  let bid = BoundID.fresh () in
  { main = bid; level = lev; }


let equal mbbid1 mbbid2 =
  BoundID.equal mbbid1.main mbbid2.main


let get_level mbbid =
  mbbid.level


let to_bound mbbid =
  mbbid.main


let pp ppf mbbid =
  Format.fprintf ppf "_%aL%d" BoundID.pp mbbid.main mbbid.level


type t = {
  main  : BoundID.t;
  name  : string;
  level : int;
}


let fresh ~message (name : string) (lev : int) : t =
  let bid = BoundID.fresh ~message:("MustBeBound, " ^ name ^ ", " ^ message) () in
  { main = bid; name = name; level = lev; }


let equal (mbbid1 : t) (mbbid2 : t) : bool =
  BoundID.equal mbbid1.main mbbid2.main


let get_level (mbbid : t) : int =
  mbbid.level


let to_bound (mbbid : t) : BoundID.t =
  mbbid.main


let pp_rich (ppf : Format.formatter) (mbbid : t) : unit =
  Format.fprintf ppf "%s" mbbid.name


let pp (ppf : Format.formatter) (mbbid : t) : unit =
  Format.fprintf ppf "_%aL%d" BoundID.pp mbbid.main mbbid.level

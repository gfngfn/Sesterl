type t = { number : int; address : Address.t; name : string }

let fresh =
  let current_max = ref 0 in
  fun (address : Address.t) (name : string) ->
    incr current_max;
    { number = !current_max; address; name }

let hash tyid = tyid.number

let compare tyid1 tyid2 = tyid2.number - tyid1.number

let equal tyid1 tyid2 = tyid1.number = tyid2.number

let name tyid = tyid.name

let address tyid = tyid.address

let pp (ppf : Format.formatter) ?(seen_from : Address.t = Address.root)
    (tyid : t) =
  let address = Address.subtract ~long:tyid.address ~short:seen_from in
  Format.fprintf ppf "%s%s" (Address.show address) tyid.name

let pp_raw (ppf : Format.formatter) (tyid : t) =
  Format.fprintf ppf "%s/%d" tyid.name tyid.number

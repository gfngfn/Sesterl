
type t = {
  number  : int;
  modules : string list;
  name    : string;
}

let fresh =
  let current_max = ref 0 in
  (fun (modules : string list) (name : string) ->
    incr current_max;
    {
      number  = !current_max;
      modules = modules;
      name    = name;
    }
  )

let hash tyid =
  tyid.number

let compare tyid1 tyid2 =
  tyid2.number - tyid1.number

let equal tyid1 tyid2 =
  tyid1.number = tyid2.number

let name tyid =
  tyid.name

let address tyid =
  tyid.modules

let pp ppf tyid =
  let prefix = tyid.modules |> List.map (fun s -> s ^ ".") |> String.concat "" in
  Format.fprintf ppf "%s%s" prefix tyid.name

let pp_raw ppf tyid =
  Format.fprintf ppf "%s/%d" tyid.name tyid.number

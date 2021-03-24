
type level = int

type t = {
          id    : int;
  mutable level : level;
}


let pp_raw ppf fid =
  Format.fprintf ppf "'%d" fid.id


let equal fid1 fid2 =
  fid1.id = fid2.id


let compare fid1 fid2 =
  fid2.id - fid1.id


let hash fid =
  fid.id


let current_max = ref 0


let initialize () =
  current_max := 0


let fresh ~message:_msg lev =
  incr current_max;
  let ret = { id = !current_max; level = lev; } in
(*
  print_endline (Format.asprintf "generate %a (%s)" pp ret msg);  (* for debug *)
*)
  ret


let get_level fid =
  fid.level


let update_level fid lev =
  fid.level <- min fid.level lev

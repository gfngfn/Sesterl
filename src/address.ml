
open MyUtil


type element =
  | Member      of string
  | FunctorBody of { arg : string }
[@@deriving show { with_path = false }]

type t = element Alist.t


let root =
  Alist.empty


let append_member (modnm : string) (address : t) =
  Alist.extend address (Member(modnm))


let append_functor_body ~arg:(modnm : string) (address : t) =
  Alist.extend address (FunctorBody{ arg = modnm })


let to_list (address : t) =
  Alist.to_list address


let subtract ~(long : t) ~(short : t) : t =
  let elems_long = Alist.to_list long in
  let elems_short = Alist.to_list short in
  let rec aux (elems_long : element list) (elems_short : element list) =
    match (elems_long, elems_short) with
    | ([], _) ->
        Alist.empty

    | (_ :: _, []) ->
        Alist.from_list elems_long

    | (elem1 :: tail1, elem2 :: tail2) ->
        begin
          match (elem1, elem2) with
          | (Member(modnm1), Member(modnm2)) ->
              if String.equal modnm1 modnm2 then
                aux tail1 tail2
              else
                Alist.from_list elems_long

          | (FunctorBody(_), FunctorBody(_)) ->
              aux tail1 tail2

          | _ ->
              Alist.from_list elems_long
        end
  in
  aux elems_long elems_short


let show (address : t) : string =
  let adelems = to_list address in
  let ss =
    adelems |> List.mapi (fun index adelem ->
      match adelem with
      | Member(modnm)  -> if index = 0 then modnm else Printf.sprintf ".%s" modnm
      | FunctorBody(r) -> Printf.sprintf "(%s = ...)" r.arg
    )
  in
  let s_last = if adelems = [] then "" else "." in
  (List.append ss [ s_last ]) |> String.concat ""


let pp (ppf : Format.formatter) (address : t) =
  let pp_sep ppf () = Format.fprintf ppf ":" in
  Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep pp_element) (to_list address)

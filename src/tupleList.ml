
open MyUtil


type 'a t = 'a * 'a * 'a list


let make x1 x2 xs =
  (x1, x2, xs)


let map f (x1, x2, xs) =
  let y1 = f x1 in
  let y2 = f x2 in
  (y1, y2, xs |> List.map f)


let map_and_fold : 'a 'b 'c. ('c -> 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t =
fun f acc0 (x1, x2, xs) ->
  let (acc1, y1) = f acc0 x1 in
  let (acc2, y2) = f acc1 x2 in
  let (acc, yacc) =
    xs |> List.fold_left (fun (acc, yacc) x ->
      let (acc, y) = f acc x in
      (acc, Alist.extend yacc y)
    ) (acc2, Alist.empty)
  in
  (acc, (y1, y2, Alist.to_list yacc))


let to_list (x1, x2, xs) =
  x1 :: x2 :: xs


let pp (type a) (ppa : Format.formatter -> a -> unit) (ppf : Format.formatter) ((x1, x2, xs) : a t) =
  Format.fprintf ppf "%a@ %a@ %a"
    ppa x1
    ppa x2
    (Format.pp_print_list ppa) xs

open MyUtil
open Syntax

type t = { target_atom : string ranged option }

let default = { target_atom = None }

let decode (attrs : attribute list) : t * attribute_warning list =
  let acc, warn_acc =
    attrs
    |> List.fold_left
         (fun (acc, warn_acc) attr ->
           let (Attribute (rng, attr_main)) = attr in
           match attr_main with
           | "atom", utast_opt -> (
               match utast_opt with
               | Some (rngs, BaseConst (BinaryByString s)) ->
                   ({ target_atom = Some (rngs, s) }, warn_acc)
               | _ ->
                   let warn =
                     {
                       position = rng;
                       tag = "atom";
                       message = "argument should be a string literal";
                     }
                   in
                   (acc, Alist.extend warn_acc warn))
           | tag, _ ->
               let warn =
                 { position = rng; tag; message = "unsupported attribute" }
               in
               (acc, Alist.extend warn_acc warn))
         (default, Alist.empty)
  in
  (acc, Alist.to_list warn_acc)

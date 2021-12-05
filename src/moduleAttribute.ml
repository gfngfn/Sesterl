open MyUtil
open Syntax

type accumulator = { acc_behaviours : StringSet.t; acc_for_test : bool }

type t = { behaviours : StringSet.t; for_test : bool }

let empty : t = { behaviours = StringSet.empty; for_test = false }

let merge (modattr1 : t) (modattr2 : t) : t =
  {
    behaviours = StringSet.union modattr1.behaviours modattr2.behaviours;
    for_test = modattr1.for_test || modattr2.for_test;
  }

let decode (attrs : attribute list) : t * attribute_warning list =
  let r = { acc_behaviours = StringSet.empty; acc_for_test = false } in
  let r, warn_acc =
    attrs
    |> List.fold_left
         (fun (r, warn_acc) attr ->
           let (Attribute (rng, attr_main)) = attr in
           match attr_main with
           | (("behaviour" | "behavior") as tag), utast_opt -> (
               match utast_opt with
               | Some (_, BaseConst (BinaryByString s)) ->
                   let r =
                     {
                       r with
                       acc_behaviours = r.acc_behaviours |> StringSet.add s;
                     }
                   in
                   (r, warn_acc)
               | _ ->
                   let warn =
                     {
                       position = rng;
                       tag;
                       message = "argument should be a string literal";
                     }
                   in
                   (r, Alist.extend warn_acc warn))
           | "test", utast_opt ->
               let warn_acc =
                 match utast_opt with
                 | None -> warn_acc
                 | Some _ ->
                     let warn =
                       {
                         position = rng;
                         tag = "test";
                         message = "argument is ignored";
                       }
                     in
                     Alist.extend warn_acc warn
               in
               let r = { r with acc_for_test = true } in
               (r, warn_acc)
           | tag, _ ->
               let warn =
                 { position = rng; tag; message = "unsupported attribute" }
               in
               (r, Alist.extend warn_acc warn))
         (r, Alist.empty)
  in
  let t = { behaviours = r.acc_behaviours; for_test = r.acc_for_test } in
  (t, Alist.to_list warn_acc)

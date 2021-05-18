
open MyUtil
open Syntax


type t = {
  is_test_suite : bool;
}


let default =
  {
    is_test_suite = false;
  }


let decode (attrs : attribute list) : t * attribute_warning list =
  let (r, warn_acc) =
    attrs |> List.fold_left (fun (r, warn_acc) attr ->
      let Attribute((rng, attr_main)) = attr in
      match attr_main with
      | ("test", utast_opt) ->
          let warn_acc =
            match utast_opt with
            | None ->
                warn_acc

            | Some(_) ->
                let warn =
                  {
                    position = rng;
                    tag      = "test";
                    message  = "argument is ignored";
                  }
                in
                Alist.extend warn_acc warn
          in
          ({ is_test_suite = true }, warn_acc)

      | (tag, _) ->
          let warn =
            {
              position = rng;
              tag      = tag;
              message  = "unsupported attribute";
            }
          in
          (r, Alist.extend warn_acc warn)

    ) (default, Alist.empty)
  in
  (r, Alist.to_list warn_acc)

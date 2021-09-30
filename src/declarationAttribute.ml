
open MyUtil
open Syntax


type t = {
  doc : string option;
}


let default =
  {
    doc = None;
  }


let decode (attrs : attribute list) : t * attribute_warning list =
  let (r, warn_acc) =
    attrs |> List.fold_left (fun (r, warn_acc) attr ->
      let Attribute((rng, attr_main)) = attr in
      match attr_main with
      | ("doc", utast_opt) ->
          begin
            match utast_opt with
            | Some((_, BaseConst(BinaryByString(s)))) ->
                ({ doc = Some(s) }, warn_acc)

            | Some((_, BaseConst(BinaryByInts(chs)))) ->
                let s = chs |> List.map Char.chr |> List.to_seq |> String.of_seq in
                ({ doc = Some(s) }, warn_acc)

            | _ ->
                let warn =
                  {
                    position = rng;
                    tag      = "doc";
                    message  = "invalid argument";
                  }
                in
                (r, Alist.extend warn_acc warn)
          end

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

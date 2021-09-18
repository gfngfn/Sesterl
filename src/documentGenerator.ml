
open MyUtil
open Syntax
open Env


type document_tree_element =
  | DocVal    of identifier * poly_type * string option
  | DocType   of type_name * type_scheme
  | DocModule of module_name * document_tree_signature
  | DocSig    of signature_name * document_tree_signature

and document_tree_signature =
  | DocStructure of document_tree_element list
  | DocFunctor   of { parameter : document_tree_element list; body : document_tree_signature }


let rec traverse_signature (modsig : module_signature) : document_tree_signature =
  match modsig with
  | ConcStructure(sigr) ->
      let docelems = traverse_structure sigr in
      DocStructure(docelems)

  | ConcFunctor(sigftor) ->
      let Domain(sigr_param) = sigftor.domain in
      let docelems_param = traverse_structure sigr_param in
      let docsig =
        let (_, modsig_body) = sigftor.codomain in
        traverse_signature modsig_body
      in
      DocFunctor{ parameter = docelems_param; body = docsig }


and traverse_structure (sigr : SigRecord.t) : document_tree_element list =
  let acc =
    sigr |> SigRecord.fold
      ~v:(fun x ventry acc ->
        Alist.extend acc (DocVal(x, ventry.val_type, ventry.val_doc))
      )
      ~c:(fun _ _ acc -> acc)
      ~f:(fun _ _ acc -> acc)
      ~t:(fun tynm tentry acc ->
        Alist.extend acc (DocType(tynm, tentry.type_scheme))
      )
      ~m:(fun modnm mentry acc ->
        let docelems = traverse_signature mentry.mod_signature in
        Alist.extend acc (DocModule(modnm, docelems))
      )
      ~s:(fun signm (_, modsig) acc ->
        let docsig = traverse_signature modsig in
        Alist.extend acc (DocSig(signm, docsig)))
      Alist.empty
  in
  acc |> Alist.to_list


let rec stringify_document_element (depth : int) (docelem : document_tree_element) : string list =
  let spec = TypeConv.display_spec_html in
  let indent = String.make (depth * 2) ' ' in
  match docelem with
  | DocVal(x, pty, doc_opt) ->
      let dispmap = DisplayMap.empty |> TypeConv.collect_ids_poly pty in
      let sty = Format.asprintf "%a" (TypeConv.pp_poly_type ~spec dispmap) pty in
      let sq =
        let acc =
          dispmap |> DisplayMap.fold_bound_id (fun bid name acc ->
            Alist.extend acc name
          ) Alist.empty
        in
        let acc =
          dispmap |> DisplayMap.fold_bound_row_id (fun brid (name, labset) acc ->
            let s_labs = labset |> LabelSet.elements |> String.concat ", " in
            Alist.extend acc (Printf.sprintf "%s :: (%s)" name s_labs)
          ) acc
        in
        match Alist.to_list acc with
        | [] -> ""
        | ss -> Printf.sprintf "&lt;%s&gt;" (String.concat ", " ss)
      in
      let s_doc =
        match doc_opt with
        | None      -> ""
        | Some(doc) -> Printf.sprintf "<div>%s</div>" doc
      in
      [ Printf.sprintf "%s<li><span>%s %s%s : %s</span>%s</li>" indent (spec.token "val") x sq sty s_doc ]

  | DocType(tynm, _) ->
      [ Printf.sprintf "%s<li><span>%s %s</span></li>" indent (spec.token "type") tynm ]

  | DocModule(modnm, docsig) ->
      let ss = docsig |> stringify_document_signature (depth + 1) in
      List.concat [
        [
          Printf.sprintf "%s<li>%s %s :" indent (spec.token "module") modnm;
        ];
        ss;
      ]

  | DocSig(signm, _docelems) ->
      [ Printf.sprintf "%s<li>%s %s</li>" indent (spec.token "signature") signm ]


and stringify_document_signature (depth : int) (docsig : document_tree_signature) : string list =
  let spec = TypeConv.display_spec_html in
  let indent = String.make (depth * 2) ' ' in
  match docsig with
  | DocStructure(docelems) ->
      List.concat [
        [
          Printf.sprintf "%s%s" indent (spec.token "sig");
          Printf.sprintf "%s<ul>" indent;
        ];
        docelems |> List.map (stringify_document_element (depth + 1)) |> List.concat;
        [
          Printf.sprintf "%s</ul>" indent;
          Printf.sprintf "%s%s" indent (spec.token "end");
        ];
      ]

  | DocFunctor{ parameter = docelems; body = docsig } ->
      List.concat [
        [
          Printf.sprintf "%s%s(%s" indent (spec.token "fun") (spec.token "sig");
          Printf.sprintf "%s<ul>" indent;
        ];
        docelems |> List.map (stringify_document_element (depth + 1)) |> List.concat;
        [
          Printf.sprintf "%s</ul>" indent;
          Printf.sprintf "%s%s) ->" indent (spec.token "end");
        ];
        docsig |> stringify_document_signature (depth + 1);
      ]


let single (out : PackageChecker.single_output) =
  let (_, sigr) = out.signature in
  DocModule(out.module_name, DocStructure(traverse_structure sigr))


let main (abspath_doc_out : absolute_path) (out : PackageChecker.single_output) : unit =
  let docelem = single out in
  let lines =
    List.concat [
      [
        "<!DOCTYPE html>";
        "<html>";
        Printf.sprintf "<head><title>%s</title></head>" out.module_name;
        "<body><ul>";
      ];
      stringify_document_element 0 docelem;
      [
        "</ul></body>";
        "</html>";
      ];
    ]
  in
  let fout = open_out abspath_doc_out in
  lines |> List.iter (fun line ->
    output_string fout line;
    output_string fout "\n"
  );
  close_out fout;
  Logging.output_written abspath_doc_out


open MyUtil
open Syntax
open Env


type document_tree_element =
  | DocVal    of identifier * poly_type * string option
  | DocType   of type_name * type_scheme
  | DocModule of module_name * document_tree_signature * string option
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
        Alist.extend acc (DocModule(modnm, docelems, None))
      )
      ~s:(fun signm (_, modsig) acc ->
        let docsig = traverse_signature modsig in
        Alist.extend acc (DocSig(signm, docsig)))
      Alist.empty
  in
  acc |> Alist.to_list


let single (out : PackageChecker.single_output) =
  let (_, sigr) = out.signature in
  DocModule(out.module_name, DocStructure(traverse_structure sigr), None)


let main (absdir_doc_out : absolute_path) (outs : PackageChecker.single_output list) : unit =
  let _docelems = outs |> List.map single in
  ()  (* TODO *)

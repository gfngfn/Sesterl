open MyUtil
open Syntax
open Env
open IntermediateSyntax

type document_tree_element_main =
  | DocVal of identifier * poly_type
  | DocType of type_name * type_scheme_with_entity
  | DocModule of module_name * document_tree_signature
  | DocSig of signature_name * document_tree_signature

and document_tree_element = document_tree_element_main * string option

and document_tree_signature =
  | DocSigVar of Address.t * signature_name
  | DocSigFunctor of
      module_name * document_tree_signature * document_tree_signature
  | DocSigWith of
      document_tree_signature * (type_name * type_scheme_with_entity) list
  | DocSigDecls of document_tree_element list

let trim_indentation (s : string) : string =
  let lines = Core.String.split_lines s in
  let acc =
    lines
    |> List.fold_left
         (fun acc line ->
           (* `res` will be:
              - `Error(n)` if the indentation depth of `line` is `n`.
              - `Ok(_)` if `line` consists only of spaces. *)
           let res =
             Core.String.fold_result s ~init:0 ~f:(fun n ch ->
                 if Char.equal ch ' ' then Ok (n + 1) else Error n)
           in
           match (acc, res) with
           | Some min_indent, Ok _ -> Some min_indent
           | Some min_indent, Error indent ->
               Some (Stdlib.min min_indent indent)
           | None, Ok _ -> None
           | None, Error indent -> Some indent)
         None
  in
  match acc with
  | None ->
      (* If `s` consists only of space lines. *)
      ""
  | Some min_indent ->
      lines
      |> List.map (fun line -> Core.String.drop_prefix line min_indent)
      |> String.concat "\n"

let rec traverse_signature (modsig : module_signature) : document_tree_signature
    =
  let isig, _ = modsig in
  traverse_signature_source isig

and traverse_signature_source (isig : signature_source) :
    document_tree_signature =
  match isig with
  | ISigVar (address, signm) -> DocSigVar (address, signm)
  | ISigWith (isig0, tydefs) ->
      let withs =
        tydefs |> List.map (fun (tynm, tentry) -> (tynm, tentry.type_scheme))
      in
      DocSigWith (traverse_signature_source isig0, withs)
  | ISigFunctor (m, isigdom, isigcod) ->
      let docsigdom = traverse_signature_source isigdom in
      let docsigcod = traverse_signature_source isigcod in
      DocSigFunctor (m, docsigdom, docsigcod)
  | ISigDecls sigr -> DocSigDecls (traverse_structure sigr)

and traverse_structure (sigr : SigRecord.t) : document_tree_element list =
  let acc =
    sigr
    |> SigRecord.fold
         ~v:(fun x ventry acc ->
           Alist.extend acc (DocVal (x, ventry.val_type), ventry.val_doc))
         ~c:(fun _ _ acc -> acc)
         ~f:(fun _ _ acc -> acc)
         ~t:(fun tynm tentry acc ->
           Alist.extend acc (DocType (tynm, tentry.type_scheme), tentry.type_doc))
         ~m:(fun modnm mentry acc ->
           let docelems = traverse_signature mentry.mod_signature in
           Alist.extend acc (DocModule (modnm, docelems), mentry.mod_doc))
         ~s:(fun signm sentry acc ->
           let _, modsig = sentry.sig_signature in
           let docsig = traverse_signature modsig in
           Alist.extend acc (DocSig (signm, docsig), sentry.sig_doc))
         Alist.empty
  in
  acc |> Alist.to_list

let stringify_type ~token:(s_token : string) ~doc:(s_doc : string)
    ~(seen_from : Address.t) (tynm : type_name)
    (tyscheme : type_scheme_with_entity) : string list =
  let spec = TypeConv.display_spec_html in
  let bids, tybody, tyentity = tyscheme in
  let dispmap =
    bids
    |> List.fold_left
         (fun dispmap bid -> dispmap |> DisplayMap.add_bound_id bid)
         DisplayMap.empty
  in
  let s_typarams =
    let ss =
      bids |> List.map (fun bid -> dispmap |> DisplayMap.find_bound_id bid)
    in
    match ss with
    | [] -> ""
    | _ :: _ -> Printf.sprintf "&lt;%s&gt;" (String.concat ", " ss)
  in
  let ss_body =
    match tyentity with
    | Opaque _tyid -> [ Printf.sprintf "<code>%s</code>" s_typarams ]
    | Synonym ->
        [
          Format.asprintf "<code>%s = %a</code>" s_typarams
            (TypeConv.pp_poly_type ~spec ~seen_from dispmap)
            tybody;
        ]
    | Variant ctormap ->
        let ss_elems =
          ConstructorMap.bindings ctormap
          |> List.map (fun (ctornm, (_, ptys)) ->
                 let s_param =
                   match ptys with
                   | [] -> ""
                   | _ :: _ ->
                       let pp_sep ppf () = Format.fprintf ppf ", " in
                       Format.asprintf "(%a)"
                         (Format.pp_print_list ~pp_sep
                            (TypeConv.pp_poly_type ~spec ~seen_from dispmap))
                         ptys
                 in
                 Printf.sprintf "<li><code>| %s%s</code></li>" ctornm s_param)
        in
        List.concat
          [
            [ Printf.sprintf "<code>%s =</code><ul>" s_typarams ];
            ss_elems;
            [ "</ul>" ];
          ]
  in
  [
    Printf.sprintf "<li><code>%s %s</code>%s%s</li>" (spec.token s_token) tynm
      (String.concat "" ss_body) s_doc;
  ]

let rec stringify_document_element ~(seen_from : Address.t)
    ((docelem, doc_opt) : document_tree_element) : string list =
  let spec = TypeConv.display_spec_html in
  let s_doc =
    match doc_opt with
    | None -> ""
    | Some doc_md_raw ->
        let doc_md = trim_indentation doc_md_raw in
        let doc_html = Omd.to_html (Omd.of_string doc_md) in
        Printf.sprintf "<div class=\"doc-area\">%s</div>" doc_html
  in
  match docelem with
  | DocVal (x, pty) ->
      let dispmap = DisplayMap.empty |> TypeConv.collect_ids_poly pty in
      let sty =
        Format.asprintf "%a"
          (TypeConv.pp_poly_type ~spec ~seen_from dispmap)
          pty
      in
      let sq =
        let acc =
          dispmap
          |> DisplayMap.fold_bound_id
               (fun bid name acc -> Alist.extend acc name)
               Alist.empty
        in
        let acc =
          dispmap
          |> DisplayMap.fold_bound_row_id
               (fun brid (name, labset) acc ->
                 let s_labs =
                   labset |> LabelSet.elements |> String.concat ", "
                 in
                 Alist.extend acc (Printf.sprintf "%s :: (%s)" name s_labs))
               acc
        in
        match Alist.to_list acc with
        | [] -> ""
        | ss -> Printf.sprintf "&lt;%s&gt;" (String.concat ", " ss)
      in
      [
        Printf.sprintf "<li><code>%s %s%s : %s</code>%s</li>" (spec.token "val")
          x sq sty s_doc;
      ]
  | DocType (tynm, tyscheme) ->
      stringify_type ~token:"type" ~doc:s_doc ~seen_from tynm tyscheme
  | DocModule (modnm, docsig) ->
      let ss =
        docsig
        |> stringify_document_signature
             ~seen_from:(seen_from |> Address.append_member modnm)
      in
      List.concat
        [
          [
            Printf.sprintf "<li><code>%s %s</code>%s<code> : </code>"
              (spec.token "module") modnm s_doc;
          ];
          ss;
          [ "</li>" ];
        ]
  | DocSig (signm, docsig) ->
      let ss = docsig |> stringify_document_signature ~seen_from in
      List.concat
        [
          [
            Printf.sprintf "<li><code>%s %s</code>%s<code> = </code>"
              (spec.token "signature") signm s_doc;
          ];
          ss;
          [ "</li>" ];
        ]

and stringify_document_signature ~(seen_from : Address.t)
    (docsig : document_tree_signature) : string list =
  let spec = TypeConv.display_spec_html in
  match docsig with
  | DocSigVar (address, signm) ->
      let diff_address = Address.subtract ~long:address ~short:seen_from in
      [ Printf.sprintf "<code>%s%s</code>" (Address.show diff_address) signm ]
  | DocSigWith (docsig0, withs) ->
      let ss1 = stringify_document_signature ~seen_from docsig0 in
      let ss2 =
        withs
        |> List.mapi (fun index (tynm, tyscheme) ->
               let token = if index = 0 then "type" else "and" in
               stringify_type ~token ~doc:"" ~seen_from tynm tyscheme)
        |> List.concat
      in
      List.concat
        [
          [ Printf.sprintf "<code>(</code>" ];
          ss1;
          [ Printf.sprintf "<code>%s</code>" (spec.token "with") ];
          ss2;
          [ Printf.sprintf "<code>)</code>" ];
        ]
  | DocSigDecls docelems ->
      List.concat
        [
          [ Printf.sprintf "<code>%s</code>" (spec.token "sig"); "<ul>" ];
          docelems
          |> List.map (stringify_document_element ~seen_from)
          |> List.concat;
          [ "</ul>"; Printf.sprintf "<code>%s</code>" (spec.token "end") ];
        ]
  | DocSigFunctor (m, docsig1, docsig2) ->
      List.concat
        [
          [ Printf.sprintf "<code>%s(%s : </code>" (spec.token "fun") m ];
          stringify_document_signature ~seen_from docsig1;
          [ Printf.sprintf "<code>) -&gt; </code>" ];
          stringify_document_signature
            ~seen_from:(seen_from |> Address.append_functor_body ~arg:m)
            docsig2;
        ]

let main (abspath_doc_out : absolute_path) (out : PackageChecker.single_output)
    : unit =
  let _, (isig, _sigr) = out.signature in
  let docelem =
    (DocModule (out.module_name, traverse_signature_source isig), None)
  in
  let lines =
    List.concat
      [
        [
          "<!DOCTYPE html>";
          "<html>";
          "<head>";
          Printf.sprintf "<title>%s</title>" out.module_name;
          "<style>";
          ".keyword { color: #0000AA; }";
          ".doc-area { background-color: #EEEEEE; padding: 2px 6px 2px 6px; \
           margin: 0px 0px 0px 0px; }";
          "</style>";
          "</head>";
          "<body><ul>";
        ];
        stringify_document_element ~seen_from:Address.root docelem;
        [ "</ul></body>"; "</html>" ];
      ]
  in
  let fout = open_out abspath_doc_out in
  lines |> List.iter (fun line -> output_string fout line);
  close_out fout;
  Logging.output_written abspath_doc_out

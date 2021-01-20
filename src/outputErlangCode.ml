
open MyUtil
open Syntax


let fresh_local_symbol () =
  OutputIdentifier.output_local (OutputIdentifier.fresh ())


type val_binding_output =
  | OBindVal         of global_name * local_name list * local_name LabelAssoc.t * (local_name * ast option) LabelAssoc.t * global_name_map * ast
  | OBindValExternal of global_name * string

type module_binding_output =
  | OBindModule of string * val_binding_output list


let traverse_val_single (gmap : global_name_map) (_, gnamefun, _, ast) : val_binding_output =
  match ast with
  | ILambda(None, lnames, mndnamemap, optnamemap, ast0) ->
      OBindVal(gnamefun, lnames, mndnamemap, optnamemap, gmap, ast0)

  | _ ->
      assert false


let make_module_string (spacepath : space_name Alist.t) : string =
  spacepath |> Alist.to_list |> List.map OutputIdentifier.output_space |> String.concat "_"


let rec traverse_binding_list (gmap : global_name_map) (spacepath : space_name Alist.t) (ibinds : binding list) : module_binding_output list * global_name_map =

  let smod = make_module_string spacepath in

  (* Associates value identifiers in the current space with `spacepath` beforehand. *)
  let gmap =
    ibinds |> List.fold_left (fun gmap ibind ->
      match ibind with
      | IBindVal(INonRec(valbind)) ->
          let (_, gnamefun, _, _) = valbind in
          gmap |> GlobalNameMap.add gnamefun smod

      | IBindVal(IRec(valbinds)) ->
          valbinds |> List.fold_left (fun gmap valbind ->
            let (_, gnamefun, _, _) = valbind in
            gmap |> GlobalNameMap.add gnamefun smod
          ) gmap

      | IBindVal(IExternal(gnamefun, _)) ->
          gmap |> GlobalNameMap.add gnamefun smod

      | IBindModule(_) ->
          gmap
    ) gmap
  in

  (* Traverses all the submodules. *)
  let (omodbindacc, gmap) =
    ibinds |> List.fold_left (fun ((omodbindacc, gmap) as original) ibind ->
      match ibind with
      | IBindVal(_) ->
          original

      | IBindModule(sname, ibindssub) ->
          let (omodbindssub, gmap) =
            let spacepathsub = Alist.extend spacepath sname in
            traverse_binding_list gmap spacepathsub ibindssub
          in
          (Alist.append omodbindacc omodbindssub, gmap)

    ) (Alist.empty, gmap)
  in

  (* Constructs the output module corresponding to the current space (if not empty). *)
  let omodbindacc =
    let ovalbinds =
      ibinds |> List.map (fun ibind ->
        match ibind with
        | IBindVal(INonRec(valbind))       -> [ traverse_val_single gmap valbind ]
        | IBindVal(IRec(valbinds))         -> valbinds |> List.map (traverse_val_single gmap)
        | IBindVal(IExternal(gname, code)) -> [ OBindValExternal(gname, code) ]
        | IBindModule(_)                   -> []
      ) |> List.concat
    in
    match ovalbinds with
    | [] ->
        omodbindacc

    | _ :: _ ->
        let omodbind = OBindModule(smod, ovalbinds) in
        Alist.extend omodbindacc omodbind
  in

  (Alist.to_list omodbindacc, gmap)


let unit_atom = "ok"


let stringify_hole = function
  | HoleC -> "c"
  | HoleF -> "f"
  | HoleE -> "e"
  | HoleG -> "g"
  | HoleS -> "s"
  | HoleP -> "p"
  | HoleW -> "w"


let stringify_format_element = function
  | FormatBreak    -> (0, "~n")
  | FormatTilde    -> (0, "~~")
  | FormatDQuote   -> (0, "\\\"")
  | FormatConst(s) -> (0, s)

  | FormatHole(hole, control) ->
      let ch = stringify_hole hole in
      let s =
        match (control.field_width, control.precision) with
        | (Some(n1), Some(n2)) -> Printf.sprintf "%d.%d" n1 n2
        | (Some(n1), None)     -> Printf.sprintf "%d" n1
        | (None, Some(n2))     -> Printf.sprintf ".%d" n2
        | (None, None)         -> ""
      in
      (1, Printf.sprintf "~%s%s" s ch)


let stringify_base_constant (bc : base_constant) =
  match bc with
  | Unit        -> unit_atom
  | Bool(true)  -> "true"
  | Bool(false) -> "false"
  | Int(n)      -> string_of_int n

  | Float(r) ->
      if Float.is_finite r then
        string_of_float r ^ "0"
          (* DOUBTFUL; are all of the string representations made in this way
             valid as constants in Erlang source? *)
      else
        assert false

  | BinaryByString(s) -> Printf.sprintf "<<\"%s\">>" (String.escaped s)
  | BinaryByInts(ns)  -> Printf.sprintf "<<%s>>" (ns |> List.map string_of_int |> String.concat ", ")
  | String(s)         -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Char(uchar)       -> Printf.sprintf "%d" (Uchar.to_int uchar)

  | FormatString(fmtelems) ->
      let pairs = fmtelems |> List.map stringify_format_element in
      let s = pairs |> List.map (fun (_, s) -> s) |> String.concat "" in
      let arity = pairs |> List.fold_left (fun arity (n, _) -> arity + n) 0 in
      Printf.sprintf "{\"%s\", %d}" s arity


let get_module_string (gmap : global_name_map) (gname : global_name) : string =
  match gmap |> GlobalNameMap.find_opt gname with
  | None       -> assert false
  | Some(smod) -> smod


let stringify_single (gmap : global_name_map) = function
  | OutputIdentifier.Local(lname) ->
      OutputIdentifier.output_local lname

  | OutputIdentifier.Global(gname) ->
      let r = OutputIdentifier.output_global gname in
      let smod = get_module_string gmap gname in
      let arity = if r.has_option then r.arity + 1 else r.arity in
      Printf.sprintf "(fun %s:%s/%d)"
        smod
        r.function_name
        arity
          (* Use syntax `fun M:F/Arity` for global function names
             in order to avoid being confused with atoms.
             Here, arities are incremented in order to conform to labeled optional parameters. *)

  | OutputIdentifier.Operator(oname) ->
      let sop = OutputIdentifier.output_operator oname in
      let s1 = fresh_local_symbol () in
      let s2 = fresh_local_symbol () in
      Printf.sprintf "(fun(%s, %s) -> %s %s %s end)" s1 s2 s1 sop s2


let make_mandatory_parameters (ordlnames : local_name list) (mndnamemap : local_name LabelAssoc.t) : local_name list =
  let mndlnames =
    mndnamemap |> LabelAssoc.bindings |> List.map (fun (_, lname) -> lname)
          (* Labeled mandatory parameters are placed in alphabetical order. *)
  in
  List.append ordlnames mndlnames


let rec stringify_option_decoding_operation (gmap : global_name_map) (sname_map : string) (optnamemap : (local_name * ast option) LabelAssoc.t) : string =
  LabelAssoc.fold (fun label (lname, default) acc ->
    let sname = OutputIdentifier.output_local lname in
    let s =
      match default with
      | None ->
          Printf.sprintf "%s = %s:%s(%s, %s), "
            sname
            Primitives.primitive_module_name
            Primitives.decode_option_function
            sname_map
            label

      | Some(ast) ->
          Printf.sprintf "%s = %s:%s(%s, %s, fun() -> %s end), "
            sname
            Primitives.primitive_module_name
            Primitives.decode_option_function_with_default
            sname_map
            label
            (stringify_ast gmap ast)
    in
    Alist.extend acc s
  ) optnamemap Alist.empty |> Alist.to_list |> String.concat ""


and stringify_arguments gmap mrow ordastargs mndargmap optargmap =
  let iter = stringify_ast gmap in
  let astargs =
    let mndastargs =
      mndargmap |> LabelAssoc.bindings |> List.map (fun (_, ast) -> ast)
        (* Labeled mandatory arguments are placed in alphabetical order. *)
    in
    List.append ordastargs mndastargs
  in
  let sargs = astargs |> List.map iter in
  let soptmap = mapify_label_assoc gmap optargmap in
  let can_take_optional = TypeConv.can_row_take_optional mrow in
  let no_mandatory_argument = (List.length astargs = 0) in
  (sargs, soptmap, can_take_optional, no_mandatory_argument)


and stringify_ast (gmap : global_name_map) (ast : ast) =
  let iter = stringify_ast gmap in
  match ast with
  | IVar(name) ->
      stringify_single gmap name

  | IBaseConst(bc) ->
      stringify_base_constant bc

  | ILambda(recopt, ordlnames, mndnamemap, optnamemap, ast0) ->
      let snames =
        let lnames = make_mandatory_parameters ordlnames mndnamemap in
        lnames |> List.map OutputIdentifier.output_local
      in
      let s0 = iter ast0 in
      let srec =
        match recopt with
        | None          -> ""
        | Some(namerec) -> " " ^ OutputIdentifier.output_local namerec
      in
      if LabelAssoc.cardinal optnamemap = 0 then
        let sparamscat = snames |> String.concat ", " in
        Printf.sprintf "fun%s(%s) -> %s end"
          srec
          sparamscat
          s0
      else
        let sparamscatcomma = snames |> List.map (fun s -> s ^ ", ") |> String.concat "" in
        let sname_map = fresh_local_symbol () in
        let sgetopts = stringify_option_decoding_operation gmap sname_map optnamemap in
        Printf.sprintf "fun%s(%s%s) -> %s%s end"
          srec
          sparamscatcomma
          sname_map
          sgetopts
          s0

  | IApply(name, mrow, ordastargs, mndargmap, optargmap) ->
      let (sargs, soptmap, can_take_optional, no_mandatory_argument) =
        stringify_arguments gmap mrow ordastargs mndargmap optargmap
      in
      begin
        match (name, sargs) with
        | (OutputIdentifier.Local(lname), _) ->
            let sname = OutputIdentifier.output_local lname in
            let sargscat = String.concat ", " sargs in
            if can_take_optional then
              if no_mandatory_argument then
                Printf.sprintf "%s(#{%s})"
                  sname
                  soptmap
              else
                Printf.sprintf "%s(%s, #{%s})"
                  sname
                  sargscat
                  soptmap
            else
              Printf.sprintf "%s(%s)"
                sname
                sargscat

        | (OutputIdentifier.Global(gname), _) ->
            let r = OutputIdentifier.output_global gname in
            let smod = get_module_string gmap gname in
            let sfun = r.function_name in
            let sopts =
              if LabelAssoc.cardinal optargmap = 0 then
                ""
                  (* When no optional argument is given, we do not output the empty map for it.
                     In response to this, functions defined with optional parameters are
                     compiled into two variants; one has its innate arity,
                     and the other can receive a map for optional arguments via an additional argument.
                  *)
              else if no_mandatory_argument then
                Printf.sprintf "#{%s}" soptmap
              else
                Printf.sprintf ", #{%s}" soptmap
            in
            Printf.sprintf "%s:%s(%s%s)"
              smod
              sfun
              (String.concat ", " sargs)
              sopts

        | (OutputIdentifier.Operator(op), [sarg1; sarg2]) ->
            let sop = OutputIdentifier.output_operator op in
            Printf.sprintf "(%s %s %s)" sarg1 sop sarg2

        | _ ->
            assert false
      end

  | IFreeze(gname, astargs) ->
      let sargs = List.map iter astargs in
      let r = OutputIdentifier.output_global gname in
      let smod = get_module_string gmap gname in
      let sfun = r.function_name in
      Printf.sprintf "{%s, %s, [%s]}"
        smod
        sfun
        (String.concat ", " sargs)

  | IFreezeUpdate(ast0, astargs) ->
      let s0 = iter ast0 in
      let sargs = List.map iter astargs in
      let varM = fresh_local_symbol () in
      let varF = fresh_local_symbol () in
      let varArgs = fresh_local_symbol () in
      Printf.sprintf "begin {%s, %s, %s} = %s, {%s, %s, %s ++ [%s]} end"
        varM
        varF
        varArgs
        s0
        varM
        varF
        varArgs
        (String.concat ", " sargs)

  | IRecord(emap) ->
      let s = mapify_label_assoc gmap emap in
      Printf.sprintf "#{%s}" s

  | IRecordAccess(ast1, label) ->
      let s1 = iter ast1 in
      Printf.sprintf "maps:get(%s, %s)" label s1

  | IRecordUpdate(ast1, label, ast2) ->
      let s1 = iter ast1 in
      let s2 = iter ast2 in
      Printf.sprintf "maps:put(%s, %s, %s)" label s2 s1

  | ILetIn(lname, ast1, ast2) ->
      let s0 = OutputIdentifier.output_local lname in
      let s1 = iter ast1 in
      let s2 = iter ast2 in
      Printf.sprintf "begin %s = %s, %s end" s0 s1 s2

  | ICase(ast1, [ IBranchCase(ipat, ast2) ]) ->
    (* -- slight optimization of case-expressions into pattern-matching let-expressions -- *)
      let spat = stringify_pattern ipat in
      let s1 = iter ast1 in
      let s2 = iter ast2 in
      Printf.sprintf "begin %s = %s, %s end" spat s1 s2

  | ICase(ast0, branches) ->
      let s0 = iter ast0 in
      let sbrs = branches |> List.map (stringify_branch_case gmap) in
      Printf.sprintf "case %s of %s end" s0 (String.concat "; " sbrs)

  | IReceive(newbrs, viabrs) ->
      let sbrs =
        let snewbrs = newbrs |> List.map (stringify_branch_new gmap) in
        let sviabrs = viabrs |> List.map (stringify_branch_via gmap) in
        List.append snewbrs sviabrs
      in
      Printf.sprintf "receive %s end" (String.concat "; " sbrs)

  | ITuple(es) ->
      let ss = es |> TupleList.to_list |> List.map iter in
      Printf.sprintf "{%s}" (String.concat ", " ss)

  | IListNil ->
      "[]"

  | IListCons(e1, e2) ->
      let s1 = iter e1 in
      let s2 = iter e2 in
      Printf.sprintf "[%s | %s]" s1 s2

  | IConstructor(ctorid, es) ->
      let sctor = ConstructorID.output ctorid in
      begin
        match es with
        | [] ->
            sctor

        | _ :: _ ->
            let ss = es |> List.map iter in
            Printf.sprintf "{%s, %s}" sctor (String.concat ", " ss)
      end

  | IThunk(e0) ->
      let s0 = iter e0 in
      Printf.sprintf "fun() -> %s end" s0

  | IForce(e0) ->
      let sname = fresh_local_symbol () in
      let s0 = iter e0 in
      Printf.sprintf "begin %s = %s, %s() end" sname s0 sname


and mapify_label_assoc (gmap : global_name_map) (emap : ast LabelAssoc.t) =
  LabelAssoc.fold (fun label ast acc ->
    let sarg = stringify_ast gmap ast in
    let s = Printf.sprintf "%s => %s" label sarg in
    Alist.extend acc s
  ) emap Alist.empty |> Alist.to_list |> String.concat ", "


and stringify_branch_case (gmap : global_name_map) (br : branch_case) =
  match br with
  | IBranchCase(pat, ast1) ->
      let spat = stringify_pattern pat in
      let s1 = stringify_ast gmap ast1 in
      Printf.sprintf "%s -> %s" spat s1


and stringify_branch_new (gmap : global_name_map) (newbr : branch_new) =
  failwith "TODO: stringify_branch_new"


and stringify_branch_via (gmap : global_name_map) (viabr : branch_via) =
  failwith "TODO: stringify_branch_via"


and stringify_pattern (ipat : pattern) =
  match ipat with
  | IPUnit        -> unit_atom
  | IPBool(true)  -> "true"
  | IPBool(false) -> "false"
  | IPInt(n)      -> string_of_int n
  | IPChar(uchar) -> string_of_int (Uchar.to_int uchar)
  | IPVar(lname)  -> OutputIdentifier.output_local lname
  | IPWildCard    -> "_"
  | IPListNil     -> "[]"

  | IPListCons(ipat1, ipat2) ->
      let s1 = stringify_pattern ipat1 in
      let s2 = stringify_pattern ipat2 in
      Printf.sprintf "[%s | %s]" s1 s2

  | IPTuple(ipats) ->
      let ss = ipats |> TupleList.to_list |> List.map stringify_pattern in
      Printf.sprintf "{%s}" (String.concat ", " ss)

  | IPConstructor(ctorid, ipats) ->
      let atom = ConstructorID.output ctorid in
      begin
        match ipats with
        | [] ->
            atom

        | _ :: _ ->
            let ss = ipats |> List.map stringify_pattern in
            Printf.sprintf "{%s, %s}" atom (String.concat ", " ss)
      end


let stringify_val_binding_output : val_binding_output -> string list = function
  | OBindVal(gnamefun, ordlnames, mndnamemap, optnamemap, gmap, ast0) ->
      let r = OutputIdentifier.output_global gnamefun in
      let sparams =
        let lnames = make_mandatory_parameters ordlnames mndnamemap in
        lnames |> List.map OutputIdentifier.output_local
      in
      let sparamscat = String.concat ", " sparams in
      let sparamscatcomma = sparams |> List.map (fun s -> s ^ ", ") |> String.concat "" in
      let sname_map = fresh_local_symbol () in
      let sgetopts = stringify_option_decoding_operation gmap sname_map optnamemap in
      let s0 = stringify_ast gmap ast0 in
      if r.has_option then
        let s_without_option =
          Printf.sprintf "%s(%s) -> ?MODULE:%s(%s#{})."
            r.function_name
            sparamscat
            r.function_name
            sparamscatcomma
        in
        let s_with_option =
          Printf.sprintf "%s(%s%s) -> %s%s."
            r.function_name
            sparamscatcomma
            sname_map
            sgetopts
            s0
        in
        [ s_without_option; s_with_option ]
      else
        let s =
          Printf.sprintf "%s(%s) -> %s."
            r.function_name
            sparamscat
            s0
        in
        [ s ]

  | OBindValExternal(_, code) ->
      [code]


let stringify_module_binding_output (omodbind : module_binding_output) : string list =
  match omodbind with
  | OBindModule(smod, ovalbinds) ->
      let exports =
        ovalbinds |> List.map (function
        | OBindVal(gnamefun, _, _, _, _, _)
        | OBindValExternal(gnamefun, _) ->
            let r = OutputIdentifier.output_global gnamefun in
            if r.has_option then
              [
                Printf.sprintf "%s/%d" r.function_name r.arity;
                Printf.sprintf "%s/%d" r.function_name (r.arity + 1);
              ]
            else
              [
                Printf.sprintf "%s/%d" r.function_name r.arity;
              ]
        ) |> List.concat
      in
      let ss = ovalbinds |> List.map stringify_val_binding_output |> List.concat in
      List.concat [
        [ Printf.sprintf "-module(%s)." smod ];
        [ Printf.sprintf "-export([%s])." (String.concat ", " exports) ];
        ss;
      ]


let write_file (dir_out : string) (smod : string) (lines : string list) : unit =
  let fpath_out = Core.Filename.concat dir_out (Printf.sprintf "%s.erl" smod) in
  let fout = open_out fpath_out in
  lines |> List.iter (fun line ->
    output_string fout (line ^ "\n")
  );
  close_out fout;
  Logging.output_written fpath_out


let write_module_to_file (dir_out : string) (omodbind : module_binding_output) : unit =
  let smod = match omodbind with OBindModule(smod, _) -> smod in
  let lines = stringify_module_binding_output omodbind in
  write_file dir_out smod lines


let write_primitive_module (dir_out : string) : unit =
  let smod = Primitives.primitive_module_name in
  let primdefs = Primitives.primitive_definitions in
  let exports =
    primdefs |> List.map (fun primdef ->
      let open Primitives in
      let targetdef = primdef.target in
      let arity = List.length targetdef.parameters in
      Printf.sprintf "%s/%d" targetdef.target_name arity
    )
  in
  let lines =
    List.concat [
      [
        Printf.sprintf "-module(%s)." smod;
        Printf.sprintf "-export([%s])." (String.concat ", " exports);
      ];
      primdefs |> List.map (fun primdef ->
        let open Primitives in
        let targetdef = primdef.target in
        Printf.sprintf "%s(%s) -> %s."
          targetdef.target_name
          (String.concat ", " targetdef.parameters)
          targetdef.code
      );
    ]
  in
  write_file dir_out smod lines


let main (dir_out : string) (gmap : global_name_map) ~package_name:(pkgnameopt : space_name option) ~module_name:(sname : space_name) (ibinds : binding list) : global_name_map =
(*
  Format.printf "OutputErlangCode | package: %a, module: %a\n"
    OutputIdentifier.pp_space pkgname
    OutputIdentifier.pp_space sname;  (* for debug *)
*)
  let (omodbinds, gmap_after) =
    let spacepath =
      match pkgnameopt with
      | Some(pkgname) -> Alist.extend (Alist.extend Alist.empty pkgname) sname
      | None          -> Alist.extend Alist.empty sname
    in
    traverse_binding_list gmap spacepath ibinds
  in
  omodbinds |> List.iter (fun omodbind ->
    write_module_to_file dir_out omodbind
  );
  gmap_after

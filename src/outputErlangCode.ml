
open MyUtil
open Syntax


type val_binding_output =
  | OBindVal of global_name * local_name list * global_name_map * ast

type module_binding_output =
  | OBindModule of string * val_binding_output list


let traverse_val_single (gmap : global_name_map) (_, gnamefun, _, ast) : val_binding_output =
  match ast with
  | ILambda(None, lnameparams, ast0) -> OBindVal(gnamefun, lnameparams, gmap, ast0)
  | _                                -> assert false


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
        | IBindVal(INonRec(valbind)) -> [ traverse_val_single gmap valbind ]
        | IBindVal(IRec(valbinds))   -> valbinds |> List.map (traverse_val_single gmap)
        | IBindModule(_)             -> []
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


let stringify_base_constant (bc : base_constant) =
  match bc with
  | Unit              -> unit_atom
  | Bool(true)        -> "true"
  | Bool(false)       -> "false"
  | Int(n)            -> string_of_int n
  | BinaryByString(s) -> Printf.sprintf "<<\"%s\">>" (String.escaped s)
  | BinaryByInts(ns)  -> Printf.sprintf "<<%s>>" (ns |> List.map string_of_int |> String.concat ", ")


let get_module_string (gmap : global_name_map) (gname : global_name) : string =
  match gmap |> GlobalNameMap.find_opt gname with
  | None       -> assert false
  | Some(smod) -> smod


let stringify_single (gmap : global_name_map) = function
  | OutputIdentifier.Local(lname) ->
      OutputIdentifier.output_local lname

  | OutputIdentifier.Global(gname) ->
      let r = OutputIdentifier.output_global gname in
      let sparam =
        List.init r.arity (fun _ ->
          let lname = OutputIdentifier.fresh () in
          OutputIdentifier.output_local lname
        ) |> String.concat ", "
      in
      let smod = get_module_string gmap gname in
      let sfun = r.function_name in
      Printf.sprintf "(fun(%s) -> %s:%s(%s) end)" sparam smod sfun sparam
        (*  Performs the eta expansion for global function names
            in order to avoid being confused with atoms.
            Note that we cannot simply use `(fun ?MODULE:F/A)` here
            because it is not valid for private functions. *)

  | OutputIdentifier.Operator(oname) ->
      let sop = OutputIdentifier.output_operator oname in
      let gensym () =
        let lname = OutputIdentifier.fresh () in
        OutputIdentifier.output_local lname
      in
      let s1 = gensym () in
      let s2 = gensym () in
      Printf.sprintf "(fun(%s, %s) -> %s %s %s end)" s1 s2 s1 sop s2


let rec stringify_ast (gmap : global_name_map) (ast : ast) =
  let iter = stringify_ast gmap in
  match ast with
  | IVar(name) ->
      stringify_single gmap name

  | IBaseConst(bc) ->
      stringify_base_constant bc

  | ILambda(recopt, names, ast0) ->
      let snames = names |> List.map OutputIdentifier.output_local in
      let s0 = iter ast0 in
      let srec =
        match recopt with
        | None          -> ""
        | Some(namerec) -> " " ^ OutputIdentifier.output_local namerec
      in
      Printf.sprintf "fun%s(%s) -> %s end" srec (String.concat ", " snames) s0

  | IApply(name, astargs) ->
      let sargs = astargs |> List.map iter in
      begin
        match (name, sargs) with
        | (OutputIdentifier.Local(lname), _) ->
            let s = OutputIdentifier.output_local lname in
            Printf.sprintf "%s(%s)" s (String.concat ", " sargs)

        | (OutputIdentifier.Global(gname), _) ->
            let r = OutputIdentifier.output_global gname in
            let smod = get_module_string gmap gname in
            let sfun = r.function_name in
            Printf.sprintf "%s:%s(%s)" smod sfun (String.concat ", " sargs)

        | (OutputIdentifier.Operator(op), [sarg1; sarg2]) ->
            let sop = OutputIdentifier.output_operator op in
            Printf.sprintf "(%s %s %s)" sarg1 sop sarg2

        | _ ->
            assert false
      end

  | ILetIn(lname, ast1, ast2) ->
      let s0 = OutputIdentifier.output_local lname in
      let s1 = iter ast1 in
      let s2 = iter ast2 in
      Printf.sprintf "begin %s = %s, %s end" s0 s1 s2

  | ICase(ast1, [ IBranch(ipat, None, ast2) ]) ->
    (* -- slight optimization of case-expressions into pattern-matching let-expressions -- *)
      let spat = stringify_pattern ipat in
      let s1 = iter ast1 in
      let s2 = iter ast2 in
      Printf.sprintf "begin %s = %s, %s end" spat s1 s2

  | ICase(ast0, branches) ->
      let s0 = iter ast0 in
      let sbrs = branches |> List.map (stringify_branch gmap) in
      Printf.sprintf "case %s of %s end" s0 (String.concat "; " sbrs)

  | IReceive(branches) ->
      let sbrs = branches |> List.map (stringify_branch gmap) in
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


and stringify_branch (gmap : global_name_map) (br : branch) =
  match br with
  | IBranch(pat, ast0opt, ast1) ->
      let spat = stringify_pattern pat in
      let swhen =
        match ast0opt with
        | None ->
            ""

        | Some(ast0) ->
            let s0 = stringify_ast gmap ast0 in
            Printf.sprintf " when %s" s0
      in
      let s1 = stringify_ast gmap ast1 in
      Printf.sprintf "%s%s -> %s" spat swhen s1


and stringify_pattern (ipat : pattern) =
  match ipat with
  | IPUnit        -> unit_atom
  | IPBool(true)  -> "true"
  | IPBool(false) -> "false"
  | IPInt(n)      -> string_of_int n
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


let stringify_val_binding_output : val_binding_output -> string = function
  | OBindVal(gnamefun, lnameparams, gmap, ast0) ->
      let r = OutputIdentifier.output_global gnamefun in
      let sparams = lnameparams |> List.map OutputIdentifier.output_local in
      let s0 = stringify_ast gmap ast0 in
      Printf.sprintf "%s(%s) -> %s." r.function_name (String.concat ", " sparams) s0


let stringify_module_binding_output (omodbind : module_binding_output) : string list =
  match omodbind with
  | OBindModule(smod, ovalbinds) ->
      let exports =
        ovalbinds |> List.map (function OBindVal(gnamefun, _, _, _) ->
          let r = OutputIdentifier.output_global gnamefun in
          Printf.sprintf "%s/%d" r.function_name r.arity
        )
      in
      let ss = ovalbinds |> List.map stringify_val_binding_output in
      List.concat [
        [ Printf.sprintf "-module(%s)." smod ];
        [ Printf.sprintf "-export([%s])." (String.concat ", " exports) ];
        ss;
      ]


let write_file (dir_out : string) (smod : string) (lines : string list) : unit =
  let fpath_out = Filename.concat dir_out (Printf.sprintf "%s.erl" smod) in
  let fout = open_out fpath_out in
  lines |> List.iter (fun line ->
    output_string fout (line ^ "\n")
  );
  close_out fout;
  Printf.printf "output written on '%s'\n" fpath_out


let write_module_to_file (dir_out : string) (omodbind : module_binding_output) : unit =
  let smod = match omodbind with OBindModule(smod, _) -> smod in
  let lines = stringify_module_binding_output omodbind in
  write_file dir_out smod lines


let write_primitive_module_to_file (dir_out : string) : unit =
  let smod = Primitives.primitive_module_name in
  let primdefs = Primitives.primitive_definitions in
  let exports =
    primdefs |> List.map (fun primdef ->
      let open Primitives in
      let arity = List.length primdef.parameters in
      Printf.sprintf "%s/%d" primdef.target_name arity
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
        Printf.sprintf "%s(%s) -> %s."
          primdef.target_name
          (String.concat ", " primdef.parameters)
          primdef.code
      );
    ]
  in
  write_file dir_out smod lines


let main (dir_out : string) (sname : space_name) (ibinds : binding list) : unit =

  Format.printf "@[<v>%a@]" (Format.pp_print_list pp_binding) ibinds;

  let (omodbinds, _) =
    let (_, gmap) = Primitives.initial_environment in
    let spacepath = Alist.extend Alist.empty sname in
    traverse_binding_list gmap spacepath ibinds
  in
  write_primitive_module_to_file dir_out;
  omodbinds |> List.iter (fun omodbind ->
    write_module_to_file dir_out omodbind
  )
(*
  let sbinds = omodbinds |> List.map stringify_module_binding_output |> List.concat in
  let lines =
  in
  lines |> List.map (fun s -> s ^ "\n") |> String.concat ""
*)

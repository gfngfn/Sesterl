
open MyUtil
open Syntax
open Env


type pattern =
  | IPUnit
  | IPBool        of bool
  | IPInt         of int
  | IPChar        of Uchar.t
      [@printer (fun ppf uchar -> Format.fprintf ppf "IPChar(%a)" pp_uchar uchar)]
  | IPVar         of local_name
  | IPWildCard
  | IPListNil
  | IPListCons    of pattern * pattern
  | IPTuple       of pattern TupleList.t
  | IPConstructor of ConstructorID.t * pattern list
[@@deriving show { with_path = false; } ]

type val_binding =
  | INonRec   of (identifier * global_name * poly_type * ast)
  | IRec      of (identifier * global_name * poly_type * ast) list
  | IExternal of global_name * string

and binding =
  | IBindVal     of val_binding
  | IBindModule  of space_name * ModuleAttribute.t * binding list

and ast =
  | IBaseConst   of base_constant
  | IVar         of name
  | ILambda      of local_name option * local_name list * local_name LabelAssoc.t * (local_name * ast option) LabelAssoc.t * ast
  | IApply       of name * mono_row * ast list * ast LabelAssoc.t * ast LabelAssoc.t
  | ILetIn       of local_name * ast * ast
  | ICase        of ast * branch list
  | IReceive     of branch list
  | ITuple       of ast TupleList.t
  | IListNil
  | IListCons    of ast * ast
  | IConstructor of ConstructorID.t * ast list
  | IRecord      of ast LabelAssoc.t
  | IRecordAccess of ast * label
  | IRecordUpdate of ast * label * ast
  | IFreeze       of global_name * ast list
  | IFreezeUpdate of ast * ast list
  | IPack         of space_name
  | IAssert       of Range.t * ast

and branch =
  | IBranch of pattern * ast


let pp_sep_comma ppf () =
  Format.fprintf ppf ",@ "


let rec pp_val_binding_sub ppf (gname, e) =
  Format.fprintf ppf "%a =@[<hov>@ %a@]@,"
    OutputIdentifier.pp_global gname
    pp_ast e


and pp_val_binding ppf = function
  | INonRec(_, gname, _, e) ->
      Format.fprintf ppf "val %a"
        pp_val_binding_sub (gname, e)

  | IRec(recbinds) ->
      let pairs = recbinds |> List.map (fun (_, gname, _, e) -> (gname, e)) in
      Format.fprintf ppf "val %a"
        (Format.pp_print_list ~pp_sep:pp_sep_comma pp_val_binding_sub) pairs

  | IExternal(gname, code) ->
      Format.fprintf ppf "val %a = external@ \"%s\"@,"
        OutputIdentifier.pp_global gname
        code


and pp_binding ppf = function
  | IBindVal(valbind) ->
      pp_val_binding ppf valbind

  | IBindModule(sname, _modattr, ibinds) ->
      Format.fprintf ppf "module %a = @[<v2>{%a}@]@,"
        OutputIdentifier.pp_space sname
        (Format.pp_print_list pp_binding) ibinds


and pp_ast ppf = function
  | IBaseConst(bc) ->
      pp_base_constant ppf bc

  | IVar(name) ->
      OutputIdentifier.pp ppf name

  | ILambda(lnamerecopt, lnameparams, mndnamemap, optnamemap, e) ->
      let snamerec =
        match lnamerecopt with
        | Some(lnamerec) -> Format.asprintf "%a" OutputIdentifier.pp_local lnamerec
        | None           -> ""
      in
      Format.fprintf ppf "\\%s(%a -{%a} ?{%a}) ->@[<hov2>@ %a@]"
        snamerec
        (Format.pp_print_list ~pp_sep:pp_sep_comma OutputIdentifier.pp_local) lnameparams
        (LabelAssoc.pp OutputIdentifier.pp_local) mndnamemap
        (LabelAssoc.pp (fun ppf (lname, astopt) ->
          match astopt with
          | None ->
              Format.fprintf ppf "%a"
                OutputIdentifier.pp_local lname

          | Some(ast) ->
              Format.fprintf ppf "%a = %a"
                OutputIdentifier.pp_local lname
                pp_ast ast
        )) optnamemap
        pp_ast e

  | IApply(name, _, eargs, mndargmap, optargmap) ->
      Format.fprintf ppf "%a@[<hov2>(%a -{%a} ?{%a})@]"
        OutputIdentifier.pp name
        (Format.pp_print_list ~pp_sep:pp_sep_comma pp_ast) eargs
        (LabelAssoc.pp pp_ast) mndargmap
        (LabelAssoc.pp pp_ast) optargmap

  | ILetIn(lname, e1, e2) ->
      Format.fprintf ppf "(let %a =@[<hov2>@ %a@]@ in@ %a)"
        OutputIdentifier.pp_local lname
        pp_ast e1
        pp_ast e2

  | ICase(e0, ibrs) ->
      Format.fprintf ppf "(case@[<hov2>@ %a@]@ of@[<hov2>@ %a@]@ end)"
        pp_ast e0
        (Format.pp_print_list pp_branch) ibrs

  | ITuple(es) ->
      Format.fprintf ppf "{%a}"
        (Format.pp_print_list ~pp_sep:pp_sep_comma pp_ast) (es |> TupleList.to_list)

  | _ ->
      Format.fprintf ppf "..."


and pp_branch ppf = function
  | IBranch(ipat, e) ->
      Format.fprintf ppf "%a ->@[<hov2>@ %a@];@ "
        pp_pattern ipat
        pp_ast e


module GlobalNameMap = Map.Make(OutputIdentifier.Global)

module SpaceNameMap = Map.Make(OutputIdentifier.Space)

type name_map = string GlobalNameMap.t * string SpaceNameMap.t
(* The type for maps tracking which module every global name belongs to.
   This is used by 'Primitives' and 'OutputErlangCode'. *)

type address = module_name Alist.t

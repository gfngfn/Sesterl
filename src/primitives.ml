
open Syntax
open Env


let primitive_module_name =
  "sesterl_internal_prim"


let decode_option_function =
  "decode_option"


let decode_option_function_with_default =
  "decode_option_with_default"


let vid_option = TypeID.Variant.fresh "option"


let vid_list = TypeID.Variant.fresh "list"


let vid_format = TypeID.Variant.fresh "format"


let vid_frozen = TypeID.Variant.fresh "frozen"


let option_type (rng : Range.t) (ty : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, DataType(TypeID.Variant(vid_option), [ty]))


let list_type (rng : Range.t) (ty : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, DataType(TypeID.Variant(vid_list), [ty]))


let format_type (rng : Range.t) (ty : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, DataType(TypeID.Variant(vid_format), [ty]))


let frozen_type (rng : Range.t) (ty : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, DataType(TypeID.Variant(vid_frozen), [ty]))


let fresh_bound () =
  let bid = BoundID.fresh () in
  KindStore.register_bound_id bid UniversalKind;
  (Range.dummy "primitives-bound", TypeVar(Bound(bid)))


let dr = Range.dummy "primitives"
let u = (dr, BaseType(UnitType))
let b = (dr, BaseType(BoolType))
let i = (dr, BaseType(IntType))
let f = (dr, BaseType(FloatType))
let c = (dr, BaseType(CharType))
let ( @-> ) tydoms tycod = (dr, FuncType(tydoms, LabelAssoc.empty, FixedRow(LabelAssoc.empty), tycod))
let eff tyrcv ty0 = (dr, EffType(Effect(tyrcv), ty0))
let pid tyrcv = (dr, PidType(Pid(tyrcv)))

let tylogic : poly_type = [b; b] @-> b
let tycomp  : poly_type = [i; i] @-> b
let tyarith : poly_type = [i; i] @-> i
let tyarith_float : poly_type = [f; f] @-> f

let tyspawn : poly_type =
  let tyrecv = fresh_bound () in
  let tyrecvnew = fresh_bound () in
  [eff tyrecvnew u] @-> eff tyrecv (pid tyrecvnew)

let tysend : poly_type =
  let tyrecv = fresh_bound () in
  let tyrecvremote = fresh_bound () in
  [pid tyrecvremote; tyrecvremote] @-> eff tyrecv u

let tyreturn : poly_type =
  let tyrecv = fresh_bound () in
  let tyres = fresh_bound () in
  [tyres] @-> eff tyrecv tyres

let tyself : poly_type =
  let tyrecv = fresh_bound () in
  eff tyrecv (pid tyrecv)

let typrintdebug : poly_type =
  let typaram = fresh_bound () in
  [typaram] @-> u


let tyformat : poly_type =
  let typaram = fresh_bound () in
  [format_type dr typaram; typaram] @-> list_type dr c


type source_definition = {
  identifier  : string;
  typ         : poly_type;
}

type target_definition = {
  target_name : string;
  parameters  : string list;
  code        : string;
}

type primitive_definition = {
  source : source_definition option;
  target : target_definition;
}


let primitive_definitions = [
  {
    source = Some{
      identifier = "spawn";
      typ        = tyspawn;
    };
    target = {
      target_name = "thunk_spawn";
      parameters  = ["F"];
      code        = "fun() -> erlang:spawn(F) end";
    };
  };
  {
    source = Some{
      identifier = "send";
      typ        = tysend;
    };
    target = {
      target_name = "thunk_send";
      parameters  = ["X"; "Y"];
      code        = "fun() -> X ! Y, ok end";
    };
  };
  {
    source = Some{
      identifier = "return";
      typ        = tyreturn;
    };
    target = {
      target_name = "thunk_return";
      parameters  = ["X"];
      code        = "fun() -> X end";
    }
  };
  {
    source = Some{
      identifier = "self";
      typ        = tyself;
    };
    target = {
      target_name = "thunk_self";
      parameters  = [];
      code        = "erlang:self()";
    };
  };
  {
    source = Some{
      identifier = "print_debug";
      typ        = typrintdebug;
    };
    target = {
      target_name = "print_debug";
      parameters  = ["X"];
      code        = "io:format(\"~p~n\", [X]), ok";
    };
  };
  {
    source = Some{
      identifier = "format";
      typ        = tyformat;
    };
    target = {
      target_name = "format";
      parameters  = ["{Fmt, Arity}"; "Arg"];
      code        = "Args = tuple_to_list(Arg), lists:flatten(io_lib:format(Fmt, Args))"
    };
  };
  {
    source = Some{
      identifier = "float";
      typ        = [i] @-> f;
    };
    target = {
      target_name = "float";
      parameters  = ["N"];
      code        = "N";
    };
  };
  {
    source = Some{
      identifier = "round";
      typ        = [f] @-> i;
    };
    target = {
      target_name = "round";
      parameters  = ["X"];
      code        = "erlang:round(X)";
    };
  };
  {
    source = Some{
      identifier = "truncate";
      typ        = [f] @-> i;
    };
    target = {
      target_name = "truncate";
      parameters  = ["X"];
      code        = "erlang:trunc(X)";
    };
  };
  {
    source = None;
    target = {
      target_name = decode_option_function;
      parameters  = ["Options"; "Key"];
      code        = "case maps:find(Key, Options) of error -> none; {ok, Value} -> {some, Value} end";
    };
  };
  {
    source = None;
    target = {
      target_name = decode_option_function_with_default;
      parameters  = ["Options"; "Key"; "Thunk"];
      code        = "case maps:find(Key, Options) of error -> Thunk(); {ok, Value} -> Value end";
    };
  };
]


let make_constructor_id ctor =
  match ConstructorID.make ctor with
  | None         -> assert false
  | Some(ctorid) -> ctorid


let add_variant_types vntdefs (tyenv, gmap) =
  let tyenv : Typeenv.t =
    vntdefs |> List.fold_left (fun tyenv vntdef ->
      let (tynm, vid, bids, ctordefs) = vntdef in
      let pkd = TypeConv.kind_of_arity (List.length bids) in
      let tyenv = tyenv |> Typeenv.add_variant_type tynm vid pkd in
      let (tyenv, ctorbrs) =
        ctordefs |> List.fold_left (fun (tyenv, ctorbrs) ctordef ->
          let (ctor, paramtys) = ctordef in
          let ctorid = make_constructor_id ctor in
          let ctorentry =
            {
              belongs         = vid;
              constructor_id  = ctorid;
              type_variables  = bids;
              parameter_types = paramtys;
            }
          in
          let tyenv = tyenv |> Typeenv.add_constructor ctor ctorentry in
          let ctorbrs = ctorbrs |> ConstructorMap.add ctor (ctorid, paramtys) in
          (tyenv, ctorbrs)
        ) (tyenv, ConstructorMap.empty)
      in
      TypeDefinitionStore.add_variant_type vid bids ctorbrs;
      tyenv
    ) tyenv
  in
  (tyenv, gmap)


let add_operators (ops : (string * poly_type * string) list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
  let tyenv =
    ops |> List.fold_left (fun tyenv (x, pty, target) ->
      let name = OutputIdentifier.Operator(OutputIdentifier.operator target) in
      tyenv |> Typeenv.add_val x pty name
    ) tyenv
  in
  (tyenv, gmap)


let add_primitives (prims : primitive_definition list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
  prims |> List.fold_left (fun (tyenv, gmap) primdef ->
    match primdef.source with
    | None ->
        (tyenv, gmap)

    | Some(srcdef) ->
        let targetdef = primdef.target in
        let gname =
          let arity = List.length targetdef.parameters in
          match OutputIdentifier.generate_global targetdef.target_name ~arity:arity ~has_option:false with
          | None        -> assert false
          | Some(gname) -> gname
        in
        let tyenv = tyenv |> Typeenv.add_val srcdef.identifier srcdef.typ (OutputIdentifier.Global(gname)) in
        let gmap = gmap |> GlobalNameMap.add gname primitive_module_name in
        (tyenv, gmap)
  ) (tyenv, gmap)


let initial_environment =
  (Typeenv.empty, GlobalNameMap.empty)
    |> add_variant_types [
      begin
        let bid = BoundID.fresh () in
        KindStore.register_bound_id bid UniversalKind;
        ("option", vid_option, [bid], [
          ("None", []);
          ("Some", [(dr, TypeVar(Bound(bid)))])
        ])
      end;
      begin
        let bid = BoundID.fresh () in
        KindStore.register_bound_id bid UniversalKind;
        ("list", vid_list, [bid], [
          (* Here is no constructor definition
             because `ListNil` and `ListCons` are provided for type `untyped_ast`. *)
        ])
      end;
      begin
        let bid = BoundID.fresh () in
        KindStore.register_bound_id bid UniversalKind;
        ("format", vid_format, [bid], [
        ])
      end;
      begin
        let bid = BoundID.fresh () in
        KindStore.register_bound_id bid UniversalKind;
        ("frozen", vid_frozen, [bid], [
        ])
      end;
    ]
    |> add_operators [
      ("&&", tylogic, "and");
      ("||", tylogic, "or" );
      ("==", tycomp , "==" );
      ("<=", tycomp , "=<" );
      (">=", tycomp , ">=" );
      ("<" , tycomp , "<"  );
      (">" , tycomp , ">"  );
      ("*" , tyarith, "*"  );
      ("/" , tyarith, "div");
      ("+" , tyarith, "+"  );
      ("-" , tyarith, "-"  );
      ("+.", tyarith_float, "+");
      ("-.", tyarith_float, "-");
      ("*.", tyarith_float, "*");
      ("/.", tyarith_float, "/");
    ]
    |> add_primitives primitive_definitions

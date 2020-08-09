
open Syntax
open Env


let primitive_module_name =
  "sesterl_internal_prim"


let decode_option_function =
  "decode_option"


let fresh_bound () =
  let bid = BoundID.fresh () in
  (Range.dummy "primitives-bound", TypeVar(Bound(bid)))


let dr = Range.dummy "primitives"
let u = (dr, BaseType(UnitType))
let b = (dr, BaseType(BoolType))
let i = (dr, BaseType(IntType))
let ( @-> ) tydoms tycod = (dr, FuncType(tydoms, FixedRow(LabelAssoc.empty), tycod))
let eff tyrcv ty0 = (dr, EffType(Effect(tyrcv), ty0))
let pid tyrcv = (dr, PidType(Pid(tyrcv)))

let tylogic : poly_type = [b; b] @-> b
let tycomp  : poly_type = [i; i] @-> b
let tyarith : poly_type = [i; i] @-> i

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
      parameters  = ["X"];
      code        = "fun() -> erlang:spawn(X) end";
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
    source = None;
    target = {
      target_name = decode_option_function;
      parameters  = ["Options"; "Key"];
      code        = "case maps:find(Key, Options) of error -> none; {ok, Value} -> {some, Value} end";
    };
  };
]


let make_constructor_id ctor =
  match ConstructorID.make ctor with
  | None         -> assert false
  | Some(ctorid) -> ctorid


let vid_option = TypeID.Variant.fresh "option"


let option_type (ty : mono_type) : mono_type =
  (dr, DataType(TypeID.Variant(vid_option), [ty]))


let initial_environment =
  let add_variant_types vntdefs (tyenv, gmap) =
    let tyenv : Typeenv.t =
      vntdefs |> List.fold_left (fun tyenv vntdef ->
        let (tynm, vid, bids, ctordefs) = vntdef in
        let tyenv = tyenv |> Typeenv.add_variant_type tynm vid (List.length bids) in
        ctordefs |> List.fold_left (fun tyenv ctordef ->
          let (ctor, paramtys) = ctordef in
          let ctorentry =
            {
              belongs         = vid;
              constructor_id  = make_constructor_id ctor;
              type_variables  = bids;
              parameter_types = paramtys;
            }
          in
          tyenv |> Typeenv.add_constructor ctor ctorentry
        ) tyenv
      ) tyenv
    in
    (tyenv, gmap)
  in
  let add_operators (ops : (string * poly_type * string) list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
    let tyenv =
      ops |> List.fold_left (fun tyenv (x, pty, target) ->
        let name = OutputIdentifier.Operator(OutputIdentifier.operator target) in
        tyenv |> Typeenv.add_val x pty name
      ) tyenv
    in
    (tyenv, gmap)
  in
  let add_primitives (prims : primitive_definition list) ((tyenv, gmap) : Typeenv.t * global_name_map) : Typeenv.t * global_name_map =
    prims |> List.fold_left (fun (tyenv, gmap) primdef ->
      match primdef.source with
      | None ->
          (tyenv, gmap)

      | Some(srcdef) ->
          let targetdef = primdef.target in
          let gname =
            let arity = List.length targetdef.parameters in
            match OutputIdentifier.generate_global targetdef.target_name arity with
            | None        -> assert false
            | Some(gname) -> gname
          in
          let tyenv = tyenv |> Typeenv.add_val srcdef.identifier srcdef.typ (OutputIdentifier.Global(gname)) in
          let gmap = gmap |> GlobalNameMap.add gname primitive_module_name in
          (tyenv, gmap)
    ) (tyenv, gmap)
  in

  (Typeenv.empty, GlobalNameMap.empty)
    |> add_variant_types [
      let bid = BoundID.fresh () in
      ("option", vid_option, [bid], [
        ("None", []);
        ("Some", [(dr, TypeVar(Bound(bid)))])
      ]);
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
    ]
    |> add_primitives primitive_definitions

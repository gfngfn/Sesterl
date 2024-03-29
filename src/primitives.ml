
open MyUtil
open Syntax
open IntermediateSyntax
open Env


let primitive_module_name =
  "sesterl_internal_prim"


let decode_option_function =
  "decode_option"


let decode_option_function_with_default =
  "decode_option_with_default"


let vid_option = TypeID.fresh Address.root "option"


let vid_result = TypeID.fresh Address.root "result"


let vid_list = TypeID.fresh Address.root "list"


let vid_format = TypeID.fresh Address.root "format"


let vid_frozen = TypeID.fresh Address.root "frozen"


let option_type (rng : Range.t) (ty : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, TypeApp(vid_option, [ty]))


let list_type (rng : Range.t) (ty : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, TypeApp(vid_list, [ty]))


let format_type (rng : Range.t) (ty : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, TypeApp(vid_format, [ty]))


let frozen_type (rng : Range.t)
    ~rest:(tyrest : ('a, 'b) typ)
    ~receive:(tyrecv : ('a, 'b) typ)
    ~return:(tycod : ('a, 'b) typ) : ('a, 'b) typ =
  (rng, TypeApp(vid_frozen, [tyrest; tyrecv; tycod]))


let assertion_function_type : mono_type =
  let dr = Range.dummy "assertion_function_type" in
  let domty =
    {
      ordered   = [(dr, BaseType(BinaryType)); (dr, BaseType(IntType))];
      mandatory = LabelAssoc.empty;
      optional  = RowEmpty;
    }
  in
  (dr, FuncType(domty, (dr, BaseType(UnitType))))


let fresh_bound () =
  let bid = BoundID.fresh () in
  (Range.dummy "primitives-bound", TypeVar(Bound(bid)))


let dr = Range.dummy "primitives"
let u = (dr, BaseType(UnitType))
let b = (dr, BaseType(BoolType))
let i = (dr, BaseType(IntType))
let f = (dr, BaseType(FloatType))
let c = (dr, BaseType(CharType))

let ( @-> ) tydoms tycod =
  let domain =
    {
      ordered   = tydoms;
      mandatory = LabelAssoc.empty;
      optional  = RowEmpty;
    }
  in
  (dr, FuncType(domain, tycod))

let eff tydoms tyrcv ty0 =
  let domain =
    {
      ordered   = tydoms;
      mandatory = LabelAssoc.empty;
      optional  = RowEmpty;
    }
  in
  (dr, EffType(domain, Effect(tyrcv), ty0))

let pid tyrcv = (dr, PidType(Pid(tyrcv)))

let tylogic : poly_type = [b; b] @-> b
let tycomp  : poly_type = [i; i] @-> b
let tyarith : poly_type = [i; i] @-> i
let tyarith_float : poly_type = [f; f] @-> f

let tyspawn : poly_type =
  let tyrecv = fresh_bound () in
  let tyrecvnew = fresh_bound () in
  eff [eff [] tyrecvnew u] tyrecv (pid tyrecvnew)

let tysend : poly_type =
  let tyrecv = fresh_bound () in
  let tyrecvremote = fresh_bound () in
  eff [pid tyrecvremote; tyrecvremote] tyrecv u

let tyreturn : poly_type =
  let tyrecv = fresh_bound () in
  let tyres = fresh_bound () in
  eff [tyres] tyrecv tyres

let tyself : poly_type =
  let tyrecv = fresh_bound () in
  eff [] tyrecv (pid tyrecv)

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
      target_name = "spawn";
      parameters  = ["F"];
      code        = "erlang:spawn(F)";
    };
  };
  {
    source = Some{
      identifier = "send";
      typ        = tysend;
    };
    target = {
      target_name = "send";
      parameters  = ["Pid"; "Msg"];
      code        = Printf.sprintf "Pid ! {%s, Msg}, ok" Constants.message_tag_atom;
    };
  };
  {
    source = Some{
      identifier = "return";
      typ        = tyreturn;
    };
    target = {
      target_name = "return";
      parameters  = ["X"];
      code        = "X";
    }
  };
  {
    source = Some{
      identifier = "self";
      typ        = tyself;
    };
    target = {
      target_name = "self";
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
      parameters  = ["{Fmt, _Arity}"; "Arg"];
      code        = "Args = case Arg of ok -> []; _ -> tuple_to_list(Arg) end, lists:flatten(io_lib:format(Fmt, Args))"
    };
  };
  {
    source = None;
    target = {
      target_name = decode_option_function;
      parameters  = ["Options"; "Key"];
      code        = "maps:find(Key, Options)";
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


let make_constructor_id (ctor : string) (atom_opt : string option) =
  match atom_opt with
  | None ->
      begin
        match ConstructorID.from_upper_camel_case ctor with
        | None         -> assert false
        | Some(ctorid) -> ctorid
      end

  | Some(atom) ->
      begin
        match ConstructorID.from_snake_case atom with
        | None         -> assert false
        | Some(ctorid) -> ctorid
      end


type constructor_definition = constructor_name * string option * poly_type list


let add_variant_types (vntdefs : (type_name * TypeID.t * BoundID.t list * constructor_definition list) list) (tyenv, gmap) =
  let tyenv : Typeenv.t =
    vntdefs |> List.fold_left (fun tyenv vntdef ->
      let (tynm, vid, bids, ctordefs) = vntdef in
      let pkd = TypeConv.kind_of_arity (List.length bids) in
      let (centryacc, ctormap) =
        ctordefs |> List.fold_left (fun (centryacc, ctormap) ctordef ->
          let (ctornm, atom_opt, paramtys) = ctordef in
          let ctorid = make_constructor_id ctornm atom_opt in
          let centry =
            {
              belongs         = vid;
              constructor_id  = ctorid;
              type_variables  = bids;
              parameter_types = paramtys;
            }
          in
          let centryacc = Alist.extend centryacc (ctornm, centry) in
          let ctormap = ctormap |> ConstructorMap.add ctornm (ctorid, paramtys) in
          (centryacc, ctormap)
        ) (Alist.empty, ConstructorMap.empty)
      in
      let tentry =
        let (bids, tybody) = TypeConv.make_opaque_type_scheme bids vid in
        {
          type_scheme = (bids, tybody, Variant(ctormap));
          type_kind   = pkd;
          type_doc    = None;
        }
      in
      let tyenv = tyenv |> Typeenv.add_type tynm tentry in
      let tyenv =
        centryacc |> Alist.to_list |> List.fold_left (fun tyenv (ctornm, centry) ->
          tyenv |> Typeenv.add_constructor ctornm centry
        ) tyenv
      in
      tyenv
    ) tyenv
  in
  (tyenv, gmap)


let add_operators (ops : (string * poly_type * string) list) ((tyenv, nmap) : Typeenv.t * name_map) : Typeenv.t * name_map =
  let tyenv =
    ops |> List.fold_left (fun tyenv (x, pty, target) ->
      let name = OutputIdentifier.Operator(OutputIdentifier.operator target) in
      tyenv |> Typeenv.add_value x pty name
    ) tyenv
  in
  (tyenv, nmap)


let add_primitives (prims : primitive_definition list) ((tyenv, nmap) : Typeenv.t * name_map) : Typeenv.t * name_map =
  prims |> List.fold_left (fun (tyenv, nmap) primdef ->
    let (gmap, smap) = nmap in
    match primdef.source with
    | None ->
        (tyenv, nmap)

    | Some(srcdef) ->
        let targetdef = primdef.target in
        let gname =
          let arity = List.length targetdef.parameters in
          match
            OutputIdentifier.generate_global
              targetdef.target_name
              ~suffix:""
              ~arity:arity
              ~has_option:false
          with
          | None        -> assert false
          | Some(gname) -> gname
        in
        let tyenv = tyenv |> Typeenv.add_value srcdef.identifier srcdef.typ (OutputIdentifier.Global(gname)) in
        let gmap = gmap |> GlobalNameMap.add gname primitive_module_name in
        (tyenv, (gmap, smap))
  ) (tyenv, nmap)


let initial_environment =
  (Typeenv.empty, (GlobalNameMap.empty, SpaceNameMap.empty))
    |> add_variant_types [
      begin
        let bid = BoundID.fresh () in
        ("option", vid_option, [bid], [
          ("None", Some("error"), []);
          ("Some", Some("ok"),    [(dr, TypeVar(Bound(bid)))]);
        ])
      end;
      begin
        let bid_ok = BoundID.fresh () in
        let bid_error = BoundID.fresh () in
        ("result", vid_result, [bid_ok; bid_error], [
          ("Ok",    None, [(dr, TypeVar(Bound(bid_ok)))]);
          ("Error", None, [(dr, TypeVar(Bound(bid_error)))]);
        ])
      end;
      begin
        let bid = BoundID.fresh () in
        ("list", vid_list, [bid], [
          (* Here is no constructor definition
             because `ListNil` and `ListCons` are provided for type `untyped_ast`. *)
        ])
      end;
      begin
        let bid = BoundID.fresh () in
        ("format", vid_format, [bid], [
        ])
      end;
      begin
        let bid1 = BoundID.fresh () in
        let bid2 = BoundID.fresh () in
        let bid3 = BoundID.fresh () in
        ("frozen", vid_frozen, [bid1; bid2; bid3], [
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

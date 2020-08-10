
type space = IdentifierScheme.t

type local =
  | ReprLocal of {
      number : int;
      hint   : IdentifierScheme.t option;
    }
  | ReprUnused

type global =
  | ReprGlobal of {
      number        : int;
      function_name : IdentifierScheme.t;
      arity         : int;
      has_option    : bool;
    }
  | ReprDummy of {
      number : int;
    }

type operator =
  | ReprOperator of string

type t =
  | Local    of local
  | Global   of global
  | Operator of operator

type global_answer = {
  function_name : string;
  arity         : int;
  has_option    : bool;
}


let space : string -> space option =
  IdentifierScheme.from_upper_camel_case


let fresh_number : unit -> int =
  let current_max = ref 0 in
  (fun () ->
    incr current_max;
    !current_max
  )


let fresh () : local =
  let n = fresh_number () in
  ReprLocal{ hint = None; number = n }


let fresh_global_dummy () : global =
  let n = fresh_number () in
  ReprDummy{
    number = n;
  }

let generate_local (s : string) : local option =
  IdentifierScheme.from_snake_case s |> Option.map (fun ident ->
    let n = fresh_number () in
    ReprLocal{ hint = Some(ident); number = n }
  )


let generate_global (s : string) ~arity:(arity : int) ~has_option:(has_option : bool) : global option =
  IdentifierScheme.from_snake_case s |> Option.map (fun ident ->
    let n = fresh_number () in
    ReprGlobal{
      number        = n;
      function_name = ident;
      arity         = arity;
      has_option    = has_option;
    }
  )


let operator (s : string) : operator =
  ReprOperator(s)


let unused : local =
  ReprUnused


module Local = struct

  type t = local


  let compare lname1 lname2 =
    match (lname1, lname2) with
    | (ReprUnused, ReprUnused)       -> 0
    | (ReprUnused, _)                -> -1
    | (_, ReprUnused)                -> 1
    | (ReprLocal(r1), ReprLocal(r2)) -> r2.number - r1.number

end


module Global = struct

  type t = global


  let compare gname1 gname2 =
    let extract_number = function
      | ReprDummy(r)  -> r.number
      | ReprGlobal(r) -> r.number
    in
    extract_number gname2 - extract_number gname1

end


let output_space =
  IdentifierScheme.to_snake_case


let output_local = function
  | ReprLocal(r) ->
      let hint =
        match r.hint with
        | None        -> ""
        | Some(ident) -> IdentifierScheme.to_upper_camel_case ident
      in
      Printf.sprintf "S%d%s" r.number hint

  | ReprUnused ->
      "_"


let output_global = function
  | ReprGlobal(r) ->
      {
        function_name = r.function_name |> IdentifierScheme.to_snake_case;
        arity         = r.arity;
        has_option    = r.has_option;
      }
  | ReprDummy(r) ->
(*
      Format.printf "attempted to output G%d(dummy)\n" r.number;  (* for debug *)
*)
      assert false


let output_operator = function
  | ReprOperator(s) ->
      s


let pp_space =
  IdentifierScheme.pp


let pp_local ppf = function
  | ReprLocal(r) ->
      begin
        match r.hint with
        | None        -> Format.fprintf ppf "L%d" r.number
        | Some(ident) -> Format.fprintf ppf "L%d%a" r.number IdentifierScheme.pp ident
      end

  | ReprUnused ->
      Format.fprintf ppf "UNUSED"


let pp_global ppf = function
  | ReprGlobal(r) ->
      Format.fprintf ppf "G%d%a/%d"
        r.number
        IdentifierScheme.pp r.function_name
        r.arity

  | ReprDummy(r) ->
      Format.fprintf ppf "G%d(dummy)"
        r.number


let pp_operator ppf = function
  | ReprOperator(s) ->
      Format.fprintf ppf "O\"%s\"" s


let pp ppf = function
  | Local(l)    -> pp_local ppf l
  | Global(g)   -> pp_global ppf g
  | Operator(o) -> pp_operator ppf o

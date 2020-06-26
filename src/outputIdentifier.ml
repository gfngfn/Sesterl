
type t =
  | ReprLocal of {
      number : int;
      hint   : IdentifierScheme.t option;
    }
  | ReprGlobal of {
      module_names  : IdentifierScheme.t list;
      function_name : IdentifierScheme.t;
      arity         : int;
    }
  | ReprOperator of string
  | ReprUnused

type answer =
  | Local of string
  | Global of {
      module_names  : string list;
      function_name : string;
      arity         : int;
    }
  | Operator of string


let is_latin_lowercase char =
  'a' <= char && char <= 'z'


let fresh_number : unit -> int =
  let current_max = ref 0 in
  (fun () ->
    incr current_max;
    !current_max
  )


let fresh () : t =
  let n = fresh_number () in
  ReprLocal{ hint = None; number = n }


let local (s : string) : t option =
  IdentifierScheme.from_snake_case s |> Option.map (fun ident ->
    let n = fresh_number () in
    ReprLocal{ hint = Some(ident); number = n }
  )


let global (s : string) (arity : int) : t option =
  IdentifierScheme.from_snake_case s |> Option.map (fun ident ->
    ReprGlobal{
      module_names  = [];
      function_name = ident;
      arity         = arity;
    }
  )


let global_operator (s : string) : t =
  ReprOperator(s)


let output (x : t) : answer =
  match x with
  | ReprLocal(r) ->
      let hint =
        match r.hint with
        | None        -> ""
        | Some(ident) -> IdentifierScheme.to_upper_camel_case ident
      in
      Local(Printf.sprintf "S%d%s" r.number hint)

  | ReprGlobal(r) ->
      Global{
        module_names  = r.module_names |> List.map IdentifierScheme.to_snake_case;
        function_name = r.function_name |> IdentifierScheme.to_snake_case;
        arity         = r.arity;
      }

  | ReprOperator(s) ->
      Operator(s)

  | ReprUnused ->
      Local("_")


let pp ppf (ans : t) =
  match ans with
  | ReprLocal(r) ->
      begin
        match r.hint with
        | None        -> Format.fprintf ppf "L%d" r.number
        | Some(ident) -> Format.fprintf ppf "L%d%a" r.number IdentifierScheme.pp ident
      end

  | ReprGlobal(r) ->
      Format.fprintf ppf "\"%a:%a/%d\""
        (Format.pp_print_list IdentifierScheme.pp) r.module_names
        IdentifierScheme.pp r.function_name
        r.arity

  | ReprOperator(s) ->
      Format.fprintf ppf "O\"%s\"" s

  | ReprUnused ->
      Format.fprintf ppf "UNUSED"


let unused : t =
  ReprUnused

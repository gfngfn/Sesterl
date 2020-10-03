
open MyUtil


type error =
  | FieldNotFound of string
  | NotAFloat
  | NotAString
  | NotAnArray
  | NotAnObject
  | OtherMessage of string

type 'a decoder = Yaml.value -> ('a, error) result


let run (d : 'a decoder) (s : string) : ('a, error) result =
  let open ResultMonad in
  match Yaml.of_string s with
  | Ok(yval)       -> d yval
  | Error(`Msg(s)) -> err (OtherMessage(s))


let succeed (a : 'a) : 'a decoder =
  fun _ -> Ok(a)


let failure (msg : string) : 'a decoder =
  fun _ -> Error(OtherMessage(msg))


let get (field : string) (d : 'a decoder) : 'a decoder =
  let open ResultMonad in
  function
  | `O(keyvals) ->
      begin
        match
          List.find_map (fun (k, v) -> if String.equal k field then Some(v) else None) keyvals
        with
        | None    -> err (FieldNotFound(field))
        | Some(v) -> d v
      end

  | _ ->
      err NotAnObject


let bind (d : 'a decoder) (df : 'a -> 'b decoder) : 'b decoder =
fun yval ->
  match d yval with
  | Ok(a)         -> df a yval
  | Error(_) as e -> e


let number : float decoder =
  let open ResultMonad in
  function
  | `Float(x) -> return x
  | _         -> err NotAFloat


let string : string decoder =
  let open ResultMonad in
  function
  | `String(x) -> return x
  | _          -> err NotAString


let list (d : 'a decoder) : ('a list) decoder =
  let open ResultMonad in
  function
  | `A(yvals) ->
      yvals |> List.fold_left (fun res yval ->
        res >>= fun acc ->
        d yval >>= fun a ->
        return (Alist.extend acc a)
      ) (return Alist.empty) >>= fun acc ->
      return (Alist.to_list acc)

  | _ ->
      err NotAnArray


let map (f : 'a -> 'b) (d : 'a decoder) : 'b decoder =
  let open ResultMonad in
  fun yval ->
    d yval >>= fun a ->
    return (f a)


let map2 (f : 'a1 -> 'a2 -> 'b) (d1 : 'a1 decoder) (d2 : 'a2 decoder) : 'b decoder =
  let open ResultMonad in
  fun yval ->
    d1 yval >>= fun a1 ->
    d2 yval >>= fun a2 ->
    return (f a1 a2)


let map3 (f : 'a1 -> 'a2 -> 'a3 -> 'b) (d1 : 'a1 decoder) (d2 : 'a2 decoder) (d3 : 'a3 decoder) : 'b decoder =
  let open ResultMonad in
  fun yval ->
    d1 yval >>= fun a1 ->
    d2 yval >>= fun a2 ->
    d3 yval >>= fun a3 ->
    return (f a1 a2 a3)

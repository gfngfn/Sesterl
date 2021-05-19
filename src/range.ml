
type real = {
  file_name    : string;
  start_line   : int;
  start_column : int;
  last_line    : int;
  last_column  : int;
}

type t =
  | Dummy of string
  | Real  of real


let pp ppf rng =
  match rng with
  | Dummy(s) ->
      Format.fprintf ppf "(%s)" s

  | Real(r) ->
      if r.start_line = r.last_line then
        Format.fprintf ppf "file '%s', line %d, characters %d-%d"
          r.file_name r.start_line r.start_column r.last_column
      else
        Format.fprintf ppf "file '%s', line %d, character %d to line %d, character %d"
          r.file_name r.start_line r.start_column r.last_line r.last_column


let from_positions (posS, posE) =
  let fname = posS.Lexing.pos_fname in
  let lnum = posS.Lexing.pos_lnum in
  let cnumS = posS.Lexing.pos_cnum - posS.Lexing.pos_bol in
  let cnumE = posE.Lexing.pos_cnum - posE.Lexing.pos_bol in
  Real{
    file_name = fname;
    start_line = lnum;
    start_column = cnumS;
    last_line = lnum;
    last_column = cnumE;
  }


let from_lexbuf lexbuf =
  let posS = Lexing.lexeme_start_p lexbuf in
  let posE = Lexing.lexeme_end_p lexbuf in
    from_positions (posS, posE)


let dummy s = Dummy(s)


let unite r1 r2 =
  match (r1, r2) with
  | (Real(_), Dummy(_))    -> r1
  | (Dummy(_), Real(_))    -> r2
  | (Dummy(s1), Dummy(s2)) -> Dummy(s1 ^ "/" ^ s2)

  | (Real(x1), Real(x2)) ->
      Real{
        file_name = x1.file_name;
        start_line = x1.start_line;
        start_column = x1.start_column;
        last_line = x2.last_line;
        last_column = x2.last_column;
      }


let get_file_name (rng : t) =
  match rng with
  | Dummy(s) -> Printf.sprintf "(%s)" s
  | Real(r)  -> r.file_name


let get_start_line (rng : t) =
  match rng with
  | Dummy(_) -> 0
  | Real(r)  -> r.start_line

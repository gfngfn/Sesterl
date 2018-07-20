
open Syntax


let main fname =
  let inc = open_in fname in
  let lexbuf = Lexing.from_channel inc in
  let utast = ParserInterface.process lexbuf in
    Format.printf "%a" pp_untyped_ast utast


let () =
  try
    Arg.parse [] main ""
  with
  | ParserInterface.Error(rng) -> Format.printf "%a: syntax error\n" Range.pp rng
  | UnidentifiedToken(rng, s)  -> Format.printf "%a: unidentified token\n" Range.pp rng
  | SeeEndOfFileInComment(rng) -> Format.printf "%a: unclosed comment\n" Range.pp rng

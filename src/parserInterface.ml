
open Syntax

exception Error of Range.t

module I = Parser.MenhirInterpreter


let k_success utast =
  utast


let k_fail chkpt =
  match chkpt with
  | I.HandlingError(penv) ->
      let rng = Range.from_positions (I.positions penv) in
      raise (Error(rng))

  | _ ->
      assert false


let process (fname : string) (lexbuf : Lexing.lexbuf) : string list * module_name ranged * untyped_module =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  I.loop_handle k_success k_fail supplier (Parser.Incremental.main lexbuf.Lexing.lex_curr_p)

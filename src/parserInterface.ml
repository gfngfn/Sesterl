
exception Error of Range.t

module I = Parser.MenhirInterpreter


let k_success utast =
  utast


let k_fail chkpt =
  (* print_endline "k_fail";  (* for debug *) *)
  match chkpt with
  | I.HandlingError(penv) ->
      let rng = Range.from_positions (I.positions penv) in
      raise (Error(rng))

  | _ -> assert false


let process lexbuf =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    I.loop_handle k_success k_fail supplier (Parser.Incremental.main lexbuf.Lexing.lex_curr_p)

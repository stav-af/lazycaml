(* open Fixtures

open Lexer.Formatter
open Lexer.Lex
open Defs.Fun


let test_lex prog prog_name = 
  let toks = lexing_simp prog fun_toks in
  Printf.printf "Tokenized %s.fun: \n %s \nto: %s" prog_name prog (display_toks toks)

let () = 
 test_lex _defs "defs" *)
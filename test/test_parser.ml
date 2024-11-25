open Fixtures

open Lexer.Formatter
open Core.Parser
open Lexer.Lex
open Defs.Fun


let test_parse prog prog_name = 
  let toks = lexing_simp prog fun_toks in
  let parse = _prog toks in
  if 
    List.exists (
      fun (_, rest) -> rest = []
    ) parse 
  then 
    Printf.printf "Successfully parsed %s" prog_name
  else 
    Printf.printf "Failed to parse %s \n" prog_name;
    List.iter (
      fun (_, rest) -> Printf.printf("%s\n") (display_toks rest)
    ) parse 
  (* Printf.printf "Parsed %s.fun: \n %s \nto: %s" prog_name prog (display_toks toks) *)

let () = 
  test_parse _add "add";
  test_parse _fact "fact";
  test_parse _defs "defs"
 
 
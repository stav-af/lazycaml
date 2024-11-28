open Fixtures

open Core.Parser
open Lexer.Lex
open Defs.Fun
open Codegen
(* open Lexer.Formatter *)

let test_compile prog = 
  let toks = lexing_simp prog fun_toks in
  let parse = _prog toks in 
  let correct = 
    List.filter (fun (_, rest) -> rest = []) parse in
  print_endline "here";
  match correct with
  | [] -> failwith "Parser failed"
  | (parse, [])::_ -> Printf.printf "%s" (compile parse)
  | _ -> failwith "no parse"
let () = 
  test_compile _defs
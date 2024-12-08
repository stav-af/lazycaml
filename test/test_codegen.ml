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
  let compiled = match correct with
  | [] -> failwith "Parser failed"
  | (parse, [])::_ -> compile parse
  | _ -> failwith "no parse" in
  
  print_endline compiled;
  let filename = Filename.temp_file "program" ".ll" in
  let oc = open_out filename in
  output_string oc compiled;
  close_out oc;
  
  let command = Printf.sprintf "lli %s" filename in
  let ic = Unix.open_process_in command in
  let output = ref "" in
  (try
     while true do
       output := !output ^ input_line ic ^ "\n"
     done
   with End_of_file -> ());
  let status = Unix.close_process_in ic in
  print_endline !output;
  status

let () = 
  let start_time = Sys.time () in
  let n = test_compile _defs in
  
  let end_time = Sys.time () in
  match n with
  | Unix.WEXITED 0 ->
      Printf.printf "Program executed successfully in: %fs \n" (end_time -. start_time)
  | Unix.WEXITED n ->
      Printf.printf "Program failed with exit code %d\n" n
  | Unix.WSIGNALED n ->
      Printf.printf "Program terminated by signal %d\n" n
  | Unix.WSTOPPED n ->
      Printf.printf "Program stopped by signal %d\n" n
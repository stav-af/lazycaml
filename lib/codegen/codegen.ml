open Defs.Ast
open Kast

open Prelude

let label_counter = ref 0
let fresh s =
  incr label_counter;
  Printf.sprintf "%s%n" s !label_counter

(*
 -- Format --
  Branching:
    br <type> %var, label %<true_jmp>, label %<false_jmp>

  Comparison
    %<varname> = icmp <cmptype> <type> %var1, %var2

  Assignment
    %<varname> = <operator> <type> <var/val>, <var/val>

  Return
    ret <type> <var/val>
*)

let s_of_aop = function 
 | SUB -> "sub i64"
 | ADD -> "add i64"
 | MULT -> "mul i64"
 | DIV -> "udiv i64"
 | MOD -> "urem i64"

let s_of_bop = function
 | GT -> "icmp sgt i64" 
 | LT -> "icmp slt i64"
 | GE -> "icmp sge i64"
 | LE -> "icmp sle i64"
 | EQ -> "icmp eq i64"
 | NE -> "icmp ne i64"

let lazy_of_aop = function
 | "sub i64" -> "sub"
 | "add i64" -> "add"
 | "mul i64" -> "mul"
 | "udiv i64" -> "div"
 | "urem i64" -> "mod"
 | "icmp eq i64" -> "eq"
 | s -> failwith (Printf.sprintf "lazy not implemented for %s" s)

let s_of_args aargs =
  let rec aux _args acc = 
    print_endline "in aux";
    match _args with
    | [] -> ""
    | a::[] -> acc ^ Printf.sprintf "i64 %s" a 
    | a::rest -> 
        let curr = acc ^ Printf.sprintf "i64 %s, " a in
        aux rest curr
    in
  aux aargs ""

let s_of_decl_args aargs =
  let rec aux _args acc = 
    print_endline "in aux";
    match _args with
    | [] -> ""
    | a::[] -> acc ^ Printf.sprintf "i64 %%%s" a 
    | a::rest -> 
        let curr = acc ^ Printf.sprintf "i64 %%%s, " a in
        aux rest curr
    in
  aux aargs ""

let rec cps e k =
  print_endline "compilation at cps";
 match e with 
 | VAR(s) -> k(KVar s)
 | VAL(i) -> k(KNum i)
 | AEXP(op, e1, e2) ->
  let label = (fresh "tmp") in
  let sop = (s_of_aop op) in
  (cps e1 (fun lhs ->
    cps e2 (fun rhs -> 
      KLet(label, KOp(sop, lhs, rhs), k(KVar label)))))
 | ITE(COMP(op, ce1, ce2), e1, e2) ->
  let label = fresh "tmp" in
  let sop = s_of_bop op in 
  (cps ce1 (fun cv1 -> 
    cps ce2 (fun cv2 -> 
      KLet(label, KOp(sop, cv1, cv2),
        KIf(label, cps e1 k, cps e2 k)))))
 | SEQ(e1, e2) ->
  (cps e1 (fun _ -> 
    cps e2 (fun y -> k(y))))
 | CALL(fname, args) ->
  let rec aux aargs vs = 
    (match aargs with 
    | [] -> 
      let label = fresh "tmp" in
      KLet(label, KCall(fname, vs), k(KVar(label)))
    | a::xa -> (cps a (fun y -> aux xa (vs@[y])))) in
  aux args []
 | WRITE_EXPR(ex) ->
    let label = fresh "tmp" in
    (cps ex (fun y -> KLet(label, KWrite(y), k(KVar(label)))))
 | _ -> failwith "not implemented"

let cpsi e = (cps e (fun x -> KReturn(x)))

let rec compile_kval kval = 
 print_endline "compile kval";
 match kval with
 | KNum(n) ->  
  let ll = fresh "lazy" in
  Printf.sprintf "  %%%s = call %%Token* @engine_lazy(i8* null, i64 1, i64 %n)" ll n, ll
 | KVar(s) -> 
  "", Printf.sprintf "%s" s
 | KOp(op, var1, var2) ->
  let setup1, ckexp1 = compile_kval var1 in 
  let setup2, ckexp2 = compile_kval var2 in 
  Printf.sprintf "%s\n%s" setup1 setup2, 
  Printf.sprintf "call %%Token* @token_%s(%%Token* %%%s, %%Token* %%%s)" (lazy_of_aop op) ckexp1 ckexp2
 | KCall(fname, args) ->
  let flabel = fresh "f_label_" in
  let floc = Printf.sprintf "  %%%s = bitcast i64 (i64*)* @%s to i8*\n" flabel fname in

  let alabel = fresh "args_" in
  let amem = Printf.sprintf "  %%%s = call i8* @malloc(i64 %n)\n" alabel ((List.length args) * 8) in

  let cast_l = fresh "casted_" in 
  let casted_amem = Printf.sprintf "  %%%s = bitcast i8* %%%s to %%Token**\n" cast_l alabel in 

  let rec aux acc index = function
    | [] -> acc
    | arg :: rest ->
      let arglabel = fresh "argn" in
      let operation =
        let setup, value = compile_kval arg in
        (Printf.sprintf "  %%%s = getelementptr %%Token*, %%Token** %%%s, i64 %d\n" arglabel cast_l index) ^
        (Printf.sprintf "%s\n  store %%Token* %%%s, %%Token** %%%s\n" setup value arglabel)
      in
      aux (acc ^ operation) (index + 1) rest
  in
  let set_mem = aux "" 0 args in

  let int_val_label = fresh "int_ptr_" in 
  let int_ptr_val = Printf.sprintf "  %%%s = ptrtoint %%Token** %%%s to i64\n" int_val_label cast_l in

  let lazy_call = Printf.sprintf "call %%Token* @engine_lazy(i8* %%%s, i64 %n, i64 %%%s)\n" flabel (List.length args) int_val_label in 

  floc ^ amem ^ casted_amem ^ set_mem ^ int_ptr_val, lazy_call
 | KWrite(kexp) ->
  let label = fresh "strict_" in
  let setup, value = compile_kval kexp in
  Printf.sprintf "%s\n  %%%s = call i64 @engine_strict(%%Token* %%%s)\n" setup label value, 
  Printf.sprintf "call i64 @printLInt(i64 %%%s)" label

let rec compile_kexp kexp = 
 print_endline "kexp";
 match kexp with 
 | KIf(varname, exp1, exp2) ->
  let if_branch = fresh "if_branch" in
  let else_branch = fresh "else_branch" in
  let sl = fresh "strict" in 
  let brval = fresh "is_true" in
  let strict = Printf.sprintf "  %%%s = call i64 @engine_strict(%%Token* %%%s)\n" sl varname in
  let brstat = Printf.sprintf "  %%%s = icmp eq i64 %%%s, 1" brval sl in
  let condition = Printf.sprintf "  br i1 %%%s, label %%%s, label %%%s\n" brval if_branch else_branch in
  strict ^
  brstat ^
  condition ^
  "\n" ^ if_branch ^ ":\n" ^
  compile_kexp(exp1) ^
  "\n" ^ else_branch ^ ":\n" ^
  compile_kexp(exp2)
 | KLet(name, expr, next) ->
  let (setup, value) = compile_kval expr in 
  (Printf.sprintf "%s\n  %%%s = %s\n" setup name value) ^
  compile_kexp next
 | KReturn(exp) ->
  let (setup, value) = compile_kval exp in
  let strict = fresh "strict_ret" in
  let str_call = Printf.sprintf "  %%%s = call i64 @engine_strict(%%Token* %%%s)\n" strict value in
  Printf.sprintf "%s\n%s\n  ret i64 %%%s\n" setup str_call strict

let rec compile_prog p =
  print_endline "cmpile prog";
  match p with 
 | DEF_SEQ(FUNC(name, args, exp), prog) ->
    let signiature = Printf.sprintf "\ndefine i64 @%s(i64* %%args) {\n" name in
    let rec aux rest i acc = 
      match rest with
      | [] -> acc
      | h::tl -> 
        let ptr = Printf.sprintf "  %%arg%n_ptr = getelementptr i64, i64* %%args, i64 %n\n" i i in
        let cast = Printf.sprintf "  %%arg%n = load i64, i64* %%arg%n_ptr\n" i i in
        let assign = Printf.sprintf "  %%%s = call %%Token* @engine_lazy(i8* null, i64 1, i64 %%arg%n)\n" h i in
        aux tl (i + 1) (acc ^ ptr ^ cast ^ assign)
      in
    let extract = aux args 0 "" in
    signiature ^
    extract ^
    compile_kexp(cpsi exp)
    ^ "}\n" ^ 
    compile_prog prog
 | MAIN(exp) ->
  "\ndefine i64 @main() {\n" ^
  compile_kexp (cps exp (fun _ -> KReturn(KNum(0)))) ^
  "}\n"

let read_file_to_string filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let compile prog = 
  prelude ^
  compile_prog prog


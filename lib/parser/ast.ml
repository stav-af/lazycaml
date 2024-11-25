type delim =
  | NONE

type bcomp = 
 | GT | LT | GE | LE | EQ | NE 

type bop = 
 | CONJ | DISJ | BEQ | BNE

type aop = 
 | SUB
 | ADD
 | MULT
 | DIV
 | MOD

type aexp = 
 | AEXP_SEQ of aexp list
 | AEXP of aop * aexp * aexp
 | VAR of string
 | VAL of int
 | ITE of bexp * aexp * aexp
 | WRITE_VAR of string
 | WRITE_EXPR of aexp

and bexp =
 | TRUE | FALSE 
 | COMP of bcomp * aexp * aexp 
 | BEXP of bop * bexp * bexp

type args =
 | ARGS of aexp list

type def = 
 | FUNC of string * args * aexp

type prog = 
 | DEF_SEQ of def * prog
 | EXP_SEQ of aexp * prog
 | EXP of aexp
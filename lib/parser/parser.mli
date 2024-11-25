open Ast

type tokens = (string * string) list
type 'a parser = tokens -> ('a * tokens) list

val _id : string parser
val _n : aexp parser
val _bexp : bexp parser
val _bterm : bexp parser
val _bop: bop parser

val _aexp : aexp parser
val _aexp_list : aexp parser
val _aterm : aexp parser
val _afinal : aexp parser

val _args : args parser
val _def : def parser
val _prog : prog parser
type kVal = 
 | KVar of string
 | KNum of int
 | KOp of string * kVal * kVal
 | KCall of string * kVal list
 | KWrite of kVal
 
type kExp = 
 | KIf of string * kExp * kExp
 | KLet of string * kVal * kExp
 | KReturn of kVal
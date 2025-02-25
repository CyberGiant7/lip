type boolExpr =
    True
  | False
  | Not of boolExpr
  | And of boolExpr * boolExpr
  | Or of boolExpr * boolExpr
  | If of boolExpr * boolExpr * boolExpr
  | Var of string

type boolDecl = (string * boolExpr) list

type boolProg = boolDecl * boolExpr



type ident = string

type name = ident Position.located

type term' = 
  | Type
  | Prod of name * term * term
  | Arr of term * term
  | Lam of name * term * term
  | App of term * term
  | Var of ident

and term = term' Position.located

type signature = (ident * term) list

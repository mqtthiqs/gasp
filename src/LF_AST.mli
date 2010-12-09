
type ident' = string

type ident = ident' Position.located

type term' = 
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term * term
  | App of term * term
  | Var of ident

and term = term' Position.located

type signature = (ident * term) list

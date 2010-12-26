
type ident = string

type term' = 
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term * term
  | App of term * term
  | Var of ident

and term = term' Position.located

type entry =
  | Decl of term

type sign = (ident * entry) list

val equals_term : term -> term -> bool
val equals_sign : sign -> sign -> bool

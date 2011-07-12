
module Definitions = Definitions.Make (struct type variable = string end)

type ident = string

type term' =
  | Type
  | Prod of ident * term * term
  | Lam of ident * term * term
  | App of term * term
  | Ident of ident
  | Def of (term option, term, term) Definitions.construct

and term = term' Position.located

type sign = (ident * term) list

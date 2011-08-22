
module Definitions = Definitions.Make (Name)

type ident = Name.variable

type term' =
  | Type
  | Prod of ident * term * term
  | Lam of ident * term * term
  | App of term * term
  | Ident of ident
  | Def of (term option, term, term) Definitions.construct

and term = term' Position.located

type sign = (ident * term) list


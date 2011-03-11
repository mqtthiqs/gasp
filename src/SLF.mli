
type ident = string

type term' =
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term * term
  | App of term * term
  | Var of ident
  | Box of term * ident * subst
  | Meta of ident

and term = term' Position.located

and subst = (ident * term) list

type entry =
  | Decl of term

type sign = (ident * entry) list

(* Comparison of terms modulo alpha- and eta-conversion *)
val equals_term : term -> term -> bool
val equals_sign : sign -> sign -> bool

module Pp : sig
  val term : Format.formatter -> term -> unit
end

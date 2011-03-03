
type ident = string

type term' = 
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term * term
  | App of term * term
  | Var of ident
  | Meta of ident

and term = term' Position.located

type entry =
  | Decl of term

type sign = (ident * entry) list

module Idmap : Map.S with type key = ident
type subst = ident Idmap.t

(* Comparison of terms modulo alpha- and eta-conversion *)
val equals_term : subst -> term -> term -> bool
val equals_sign : subst -> sign -> sign -> bool

module Pp : sig
  val term : Format.formatter -> term -> unit
end

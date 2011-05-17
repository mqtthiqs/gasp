
type ident = string

type term' =
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term * term
  | App of term * term
  | Ident of ident
  | Box of term * (ident * int) option * term

and term = term' Position.located

type entry =
  | Decl of term

type sign = (ident * entry) list

(* Comparison of terms modulo alpha- and eta-conversion *)
val equals_term : term -> term -> bool
val equals_sign : sign -> sign -> bool

module Pp : sig
  val term : Format.formatter -> term -> unit
end

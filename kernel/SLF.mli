
type ident = string

type term' =
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term
  | App of term * term
  | Ident of ident
  | Box of term * (ident * int) option * term

and term = term' Position.located

type sign = (ident * term) list

module Pp : sig
  val term : Format.formatter -> term -> unit
end

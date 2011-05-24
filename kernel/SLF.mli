
type ident = string

type term' =
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term
  | App of term * term
  | Ident of ident
  | Box of term * (ident * ident) option * term

and term = term' Position.located

type sign = (ident * term) list

module Pp : sig
  open Print
  val term : term printing_fun
  val args : term list printing_fun
  val sign : sign printing_fun
end

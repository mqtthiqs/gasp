type term =
  | Type
  | Prod of string option * term * term
  | Lam of string option * term
  | App of term * term
  | Ident of string
  | Meta of string * term list

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of (term list -> term)

type sign = (string * term * entry_type) list

module Printer : sig
  open Format
  val term : formatter -> term -> unit
  val sign : formatter -> sign -> unit
end


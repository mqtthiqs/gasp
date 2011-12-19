type term =
  | Type
  | Prod of string option * term * term
  | App of term * term
  | Ident of string
  | Meta of string

type sign =
  | Nil
  | Cons of string * term * sign

module Printer : sig
  open Format
  val term : formatter -> term -> unit
  val sign : formatter -> sign -> unit
end


type term =
  | Type
  | Prod of string option * term * term
  | Lam of string option * term
  | App of term * term
  | Ident of string
  | Meta of string * term list

type sign = (string * term * bool * (term list -> term) option) list

module Parser : sig
  open Camlp4.PreCast
  val expand_term_quot : Ast.expr Syntax.Quotation.expand_fun
  val expand_sign_quot : Ast.expr Syntax.Quotation.expand_fun
end

module Printer : sig
  open Format
  val term : formatter -> term -> unit
  val sign : formatter -> sign -> unit
end


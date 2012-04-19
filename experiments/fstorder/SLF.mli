open Struct

type binder = string option

type ident =
  | Id of string
  | Unnamed of int
  | Unbound of int

type term =
  | Type
  | Prod of binder * term * term
  | Lam of binder * term
  | App of term * term
  | Ident of ident
  | Meta of string * term list

type lenv = (binder * term) list

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of ((lenv -> term -> term) -> term list -> term)

type sign = (string * term * entry_type) list

module Printer : sig
  open Format
  val term : formatter -> term -> unit
  val eobj : binder list -> formatter -> LF.obj -> unit
  val efam : binder list -> formatter -> LF.fam -> unit
  val ekind : binder list -> formatter -> LF.kind -> unit
  val espine : binder list -> formatter -> LF.spine -> unit
  val obj : formatter -> LF.obj -> unit
  val fam : formatter -> LF.fam -> unit
  val kind : formatter -> LF.kind -> unit
  val spine : formatter -> LF.spine -> unit
  val sign : formatter -> Struct.sign -> unit
  val env : formatter -> env -> unit
  val esubst : binder list -> formatter -> LF.subst -> unit
  val subst : formatter -> LF.subst -> unit
  val lenv : formatter -> lenv -> unit
  val context : formatter -> Context.t -> unit
  val repo : formatter -> repo -> unit
  val repo_light : formatter -> repo -> unit
end

module Strat : sig

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  val term : Struct.sign -> binder list -> term -> entity
  val obj : Struct.sign -> binder list -> term -> LF.obj
  val fam : Struct.sign -> binder list -> term -> LF.fam
  val kind : Struct.sign -> binder list -> term -> LF.kind
  val entry_type : entry_type -> Sign.entry_type
  val env : Struct.sign -> Struct.env -> lenv -> env
end

module Unstrat : sig
  val obj : binder list -> LF.obj -> term
  val fam : binder list -> LF.fam -> term
  val kind : binder list -> LF.kind -> term
  val env : env -> lenv
end

open Struct

type binder = string option

type term =
  | Type
  | Prod of binder * term * term
  | Lam of binder * term
  | App of term * term
  | Ident of string
  | Meta of string * term list

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of (Repo.t -> term list -> term)

type sign = (string * term * entry_type) list

module Printer : sig
  open Format
  val term : formatter -> term -> unit
  val eobj : binder list -> formatter -> LF.obj -> unit
  val efam : binder list -> formatter -> LF.fam -> unit
  val ekind : binder list -> formatter -> LF.kind -> unit
  val obj : formatter -> LF.obj -> unit
  val fam : formatter -> LF.fam -> unit
  val kind : formatter -> LF.kind -> unit
  val sign : formatter -> Sign.t -> unit
  val env : formatter -> Env.t -> unit
  val context : formatter -> Context.t -> unit
  val repo : formatter -> Repo.t -> unit
  val repo_light : formatter -> Repo.t -> unit
end

module Strat : sig

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  val term : Sign.t -> binder list -> term -> entity
  val obj : Sign.t -> binder list -> term -> LF.obj
  val fam : Sign.t -> binder list -> term -> LF.fam
  val kind : Sign.t -> binder list -> term -> LF.kind
  val entry_type : entry_type -> Sign.entry_type
  val env : Sign.t -> (binder * term) list -> Env.t
end

module Unstrat : sig
  val obj : binder list -> LF.obj -> term
  val fam : binder list -> LF.fam -> term
  val kind : binder list -> LF.kind -> term
end


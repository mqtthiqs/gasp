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
  val eobj : string option list -> formatter -> LF.obj -> unit
  val efam : string option list -> formatter -> LF.fam -> unit
  val ekind : string option list -> formatter -> LF.kind -> unit
  val obj : formatter -> LF.obj -> unit
  val fam : formatter -> LF.fam -> unit
  val kind : formatter -> LF.kind -> unit
  val sign : formatter -> LF.Sign.t -> unit
  val env : formatter -> LF.Env.t -> unit
end

module Strat : sig

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  val term : LF.Sign.t -> string option list -> term -> entity
  val obj : LF.Sign.t -> string option list -> term -> LF.obj
  val fam : LF.Sign.t -> string option list -> term -> LF.fam
  val kind : LF.Sign.t -> string option list -> term -> LF.kind
  val entry_type : LF.Sign.t -> entry_type -> LF.entry_type
end

module Unstrat : sig
  val obj : string option list -> LF.obj -> term
  val fam : string option list -> LF.fam -> term
  val kind : string option list -> LF.kind -> term
  val fn : LF.Sign.t -> (LF.obj list -> LF.obj) -> term list -> term
end


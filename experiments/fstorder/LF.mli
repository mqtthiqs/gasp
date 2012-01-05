open Names

type fam =
  | FApp of FConst.t * obj list
  | FProd of string option * fam * fam

and obj =
  | OApp of OConst.t * obj list
  | OVar of int
  | OMeta of Meta.t

type kind =
  | KType
  | KProd of string option * fam * kind

type env = fam list

module Env : sig
  type t
  val empty : t
  val find : int -> t -> fam
  val add : fam -> t -> t
end

module Sign : sig

  type entry =
    | OConst of fam
    | FConst of kind

  type t
  val empty : t
  val ofind : OConst.t -> t -> fam
  val ffind : FConst.t -> t -> kind
  val oadd : OConst.t -> fam -> t -> t
  val fadd : FConst.t -> kind -> t -> t
end

module Subst : sig
  val obj : obj -> obj -> obj
  val fam : obj -> fam -> fam
  val kind : obj -> kind -> kind
end

module Strat : sig

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj

  open SLF
  val term : Sign.t -> string option list -> term -> entity
  val obj : Sign.t -> string option list -> term -> obj
  val fam : Sign.t -> string option list -> term -> fam
  val kind : Sign.t -> string option list -> term -> kind
  val sign : Sign.t -> sign -> Sign.t
end

module Unstrat : sig
  open SLF
  val obj : string option list -> obj -> term
  val fam : string option list -> fam -> term
  val kind : string option list -> kind -> term
end

module Util : sig
  val fold_meta : (Meta.t -> obj) -> obj -> obj
end

module Printer : sig
  open Format
  val obj : formatter -> obj -> unit
  val fam : formatter -> fam -> unit
  val kind : formatter -> kind -> unit
  val entity : formatter -> Strat.entity -> unit
  val sign : formatter -> Sign.t -> unit
end

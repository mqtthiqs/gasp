open Names

type fam =
  | FApp of FConst.t * obj list
  | FProd of string option * fam * fam

and obj =
  | OLam of string * obj
  | OApp of head * spine
  | OMeta of Meta.t * subst

and spine = obj list
and subst = obj list

and head =
  | HVar of int
  | HConst of OConst.t

type kind =
  | KType
  | KProd of string option * fam * kind

type env = fam list

module Env : sig
  type t
  val empty : t
  val find : int -> t -> fam
  val add : fam -> t -> t
  val to_list : t -> fam list
end

module Sign : sig
  type t
  val empty : t
  val slices : OConst.t -> t -> bool
  val ofind : OConst.t -> t -> fam
  val ffind : FConst.t -> t -> kind
  val oadd : OConst.t -> bool * fam -> t -> t
  val fadd : FConst.t -> kind -> t -> t
end

module Subst : sig
  val obj : int -> obj -> obj -> obj
  val fam : int -> obj -> fam -> fam
  val kind : int -> obj -> kind -> kind
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
  val env : formatter -> Env.t -> unit
end

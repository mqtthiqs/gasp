open Names

type fam =
  | FApp of FConst.t * obj list
  | FProd of string option * fam * fam

and obj
and spine = obj list
and subst = obj list

and head =
  | HVar of int
  | HConst of OConst.t

type kind =
  | KType
  | KProd of string option * fam * kind

type cobj =
  | OLam of string option * obj
  | OApp of head * spine
  | OMeta of Meta.t * subst

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of (obj list -> obj)

val inj : cobj -> obj
val prj : obj -> cobj
val (~~) : (cobj -> cobj) -> obj -> obj

module Env : sig
  type t
  val empty : t
  val find : int -> t -> fam
  val add : string option -> fam -> t -> t
  val length : t -> int
  val to_list : t -> (string option * fam) list
  val names_of : t -> string option list
end

module Sign : sig
  type t
  val empty : t
  val ofind : OConst.t -> t -> fam * entry_type
  val ffind : FConst.t -> t -> kind
  val oadd : OConst.t -> fam * entry_type -> t -> t
  val fadd : FConst.t -> kind -> t -> t
end

module Lift : sig
  val obj : int -> int -> obj -> obj
  val fam : int -> int -> fam -> fam
end

module Subst : sig
  val obj : obj list -> obj -> obj
  val fam : obj list -> fam -> fam
  val kind : obj list -> kind -> kind
end

module Strat : sig

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj

  val term : Sign.t -> string option list -> SLF.term -> entity
  val obj : Sign.t -> string option list -> SLF.term -> obj
  val fam : Sign.t -> string option list -> SLF.term -> fam
  val kind : Sign.t -> string option list -> SLF.term -> kind
  val entry_type : Sign.t -> SLF.entry_type -> entry_type
end

module Unstrat : sig
  open SLF
  val obj : string option list -> obj -> term
  val fam : string option list -> fam -> term
  val kind : string option list -> kind -> term
  val fn : Sign.t -> (obj list -> obj) -> term list -> term
end

module Util : sig
  val map_meta : (Meta.t -> subst -> obj) -> obj -> obj
end

module Printer : sig
  open Format
  val eobj : string option list -> formatter -> obj -> unit
  val efam : string option list -> formatter -> fam -> unit
  val ekind : string option list -> formatter -> kind -> unit
  val obj : formatter -> obj -> unit
  val fam : formatter -> fam -> unit
  val kind : formatter -> kind -> unit
  val entity : formatter -> Strat.entity -> unit
  val sign : formatter -> Sign.t -> unit
  val env : formatter -> Env.t -> unit
end

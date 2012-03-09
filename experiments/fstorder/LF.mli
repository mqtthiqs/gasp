open Names

type binder = string option

type head =
  | HVar of int
  | HConst of OConst.t

type obj
and spine = obj list
and subst = obj list

type fam =
  | FApp of FConst.t * obj list
  | FProd of binder * fam * fam

type kind =
  | KType
  | KProd of binder * fam * kind

type cobj =
  | OLam of binder * obj
  | OApp of head * spine
  | OMeta of Meta.t * subst

val inj : cobj -> obj
val prj : obj -> cobj

module Lift : sig
  val obj : int -> int -> obj -> obj
  val fam : int -> int -> fam -> fam
end

module Subst : sig
  val spine : obj -> obj list -> obj
  val obj : obj list -> obj -> obj
  val fam : obj list -> fam -> fam
  val kind : obj list -> kind -> kind
end

module Util : sig
  val map_meta : (Meta.t -> subst -> obj) -> obj -> obj
  val fv : obj -> int list
  val eta_expand_var : int -> fam -> obj
end

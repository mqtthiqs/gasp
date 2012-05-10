open Names

type binder = string option

type head =
  | HVar of int
  | HConst of OConst.t
  | HInv of OConst.t * int

type obj
and spine = obj list
and subst = obj list

type fam =
  | FApp of FConst.t * spine
  | FProd of binder * fam * fam

type kind =
  | KType
  | KProd of binder * fam * kind

type cobj =
  | OLam of binder * obj
  | OApp of head * spine
  | OMeta of Meta.t * subst

exception Not_eta of obj * spine

val inj : cobj -> obj
val prj : obj -> cobj

(* Direct constructors (sugar) *)
val mkApp : head * spine -> obj
val mkLam : binder * obj -> obj
val mkMeta : Meta.t * subst -> obj

module Lift : sig
  val obj : int -> int -> obj -> obj
  val fam : int -> int -> fam -> fam
end

module Lower : sig
  val fam : int -> fam -> fam
end

module Subst : sig
  val spine : obj -> obj list -> obj
  val obj : obj list -> obj -> obj
  val fam : obj list -> fam -> fam
  val kind : obj list -> kind -> kind
end

module Util : sig
  val map_meta : (Meta.t * subst -> obj) -> obj -> obj
  val fv : obj -> int list
  val eta_expand_var : int -> fam -> obj
end

(* debug *)
module ESubst : sig
  val obj : obj Esubst.subs -> obj -> cobj
  val spine : cobj * spine -> cobj
end

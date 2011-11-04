open Names

type head =
  | HConst of Names.OConst.t
  | HVar of int

type fam =
  | FApp of Names.FConst.t * obj list
  | FArr of fam * fam
  | FProd of fam * fam

and obj =
  | OApp of spine
  | OMeta of Meta.t

and spine = head * obj list

type kind =
  | KType
  | KProd of fam * kind

type env = fam list

type entry =
  | OConst of fam
  | FConst of kind

module Subst : sig
  val obj : spine -> obj -> obj
  val fam : spine -> fam -> fam
  val kind : spine -> kind -> kind
end

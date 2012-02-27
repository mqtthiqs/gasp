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

val inj : cobj -> obj
val prj : obj -> cobj

module Env : sig
  type t
  val empty : t
  val find : int -> t -> fam
  val add : string option -> fam -> t -> t
  val length : t -> int
  val to_list : t -> (string option * fam) list
  val names_of : t -> string option list
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

module Util : sig
  val map_meta : (Meta.t -> subst -> obj) -> obj -> obj
end

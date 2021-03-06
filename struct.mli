open Names
open LF

module Env : sig
  type t = (binder * fam) list
  val find : int -> t -> fam
  val add : binder -> fam -> t -> t
  val names_of : t -> binder list
end

module Context : sig
  type t
  val empty : t
  val find : Meta.t -> t -> Env.t * obj * fam
  val add : Meta.t -> Env.t * obj * fam -> t -> t
  val fold : (Meta.t -> Env.t * obj * fam -> 'a -> 'a) -> t -> 'a -> 'a
  val fresh : t -> Meta.t * t
end

module rec Repo : sig
  type t = {
    sign: Sign.t;
    ctx: Context.t;
    head: Meta.t * subst;
  }

  val empty : t
  val fresh : t -> Meta.t * t

end

and Sign : sig

  type  entry_type =
    | Sliceable
    | Non_sliceable
    | Defined of (Repo.t -> Env.t -> (Repo.t -> Env.t -> obj -> Repo.t * obj) -> spine -> Repo.t * obj)

  type t
  val empty : t
  val ofind : OConst.t -> t -> fam * entry_type
  val ffind : FConst.t -> t -> kind
  val oadd : OConst.t -> fam * entry_type -> t -> t
  val fadd : FConst.t -> kind -> t -> t
  val fold :
    (OConst.t -> fam * entry_type -> 'a -> 'a) ->
    (FConst.t -> kind -> 'a -> 'a) -> t -> 'a -> 'a
end

type env = Env.t
type sign = Sign.t
type repo = Repo.t

module Renaming : sig
  type t = int list
  val inverse : t -> t
  val subst_of : env -> t -> subst
  val drop_env : t -> env -> env
end

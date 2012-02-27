open Names
open LF

module Context : sig
  type t
  val empty : t
  val find : Names.Meta.t -> t -> LF.Env.t * LF.obj * LF.fam
  val add : Names.Meta.t -> LF.Env.t * LF.obj * LF.fam -> t -> t
end

module rec Repo : sig
  type t = {
    sign: Sign.t;
    ctx: Context.t;
    head: Names.Meta.t;
  }

  val empty : t

  module Printer : sig
    val t : Format.formatter -> t -> unit
    val t_light : Format.formatter -> t -> unit
  end

end

and Sign : sig

  type  entry_type =
    | Sliceable
    | Non_sliceable
    | Defined of (Repo.t -> obj list -> obj)

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

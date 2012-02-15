module Context : sig
  type t
  val empty : t
  val find : Names.Meta.t -> t -> LF.obj * LF.fam
  val add : Names.Meta.t -> LF.obj * LF.fam -> t -> t
end

type t = {
  sign: LF.Sign.t;
  ctx: Context.t;
  head: Names.Meta.t;
}

module Printer : sig
  val t : Format.formatter -> t -> unit
  val t_light : Format.formatter -> t -> unit
end

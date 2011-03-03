open Name

type variable = Name.variable
type constant = Name.constant

module type SET = sig
  type t
  type value
  val add : variable -> value -> t -> t
  val find : variable -> t -> value
  val fold : (variable -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val is_empty : t -> bool
  val empty : t
end

module rec NLF : sig
  type env = NLFEnv.t
  type subst = NLFSubst.t
  type args = NLFArgs.t

  type ohead =
    | HVar of variable
    | HConst of constant
    | HDef of variable

  type kind = 
    | KType of env

  and fam = 
    | Fam of env * subst * constant * args

  and obj =
    | Obj of env * subst * ohead * args * constant * args

  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
end

and NLFEnv : (SET with type value = NLF.fam)
and NLFSubst : (SET with type value = NLF.ohead * NLFArgs.t * constant * NLFArgs.t)
and NLFSign : (SET with type value = NLF.entry)
and NLFArgs : (SET with type value = NLF.obj)

module Pp : sig
  open NLF
  open Format
  val sign : formatter -> NLFSign.t -> unit
  val obj : formatter -> obj -> unit
  val fam : formatter -> fam -> unit
  val kind : formatter -> kind -> unit
  val entry : formatter -> entry -> unit
end

val lift : NLF.obj -> NLF.fam

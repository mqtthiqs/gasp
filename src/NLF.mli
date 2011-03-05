open Name

module type SET = sig
  type t
  type key
  type value
  val add : key -> value -> t -> t
  val find : key -> t -> value
  val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
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

  type kind = 
    | KType of env

  and fam = 
    | Fam of env * subst * constant * args

  and obj =
    | Obj of env * subst * ohead * args * constant * args
    | OMeta of env * subst * definition * constant * args

  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
end

and NLFEnv : (SET with type key = variable and type value = NLF.fam)
and NLFSubst : (SET with type key = definition and type value = NLF.ohead * NLFArgs.t * constant * NLFArgs.t)
and NLFSign : (SET with type key = constant and type value = NLF.entry)
and NLFArgs : (SET with type key = variable and type value = NLF.obj)

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

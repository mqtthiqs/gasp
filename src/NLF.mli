open Name

type variable = Name.variable
type constant = Name.constant

module rec NLF : sig
  type env = NLFEnv.t
  type subst = NLFSubst.t
  type sign = NLFSign.t

  type kind = 
    | KType of env			(* Γ type *)

  and fam = 
    | Fam of env * subst * constant * subst	     (* Γ,σ ⊢ a σ *)

  and obj =
    | Obj of env * subst * ohead * subst * constant * subst     (* Γ,σ ⊢ h σ : a σ *)

  and ohead =
    | HVar of variable
    | HConst of constant
    | HObj of obj
end

and NLFEnv : sig
  type t
  val add : variable -> NLF.fam -> t -> t
  val find : variable -> t -> NLF.fam
  val fold : (variable -> NLF.fam -> 'a -> 'a) -> t -> 'a -> 'a
  val is_empty : t -> bool
  val empty : t
end

and NLFSubst : sig
  type t
  val add : variable -> NLF.obj -> t -> t
  val find : variable -> t -> NLF.obj
  val fold : (variable -> NLF.obj -> 'a -> 'a) -> t -> 'a -> 'a
  val is_empty : t -> bool
  val empty : t
end

and NLFSign : sig
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
  type t
  val add : constant -> entry -> t -> t
  val find : constant -> t -> entry
  val fold : (constant -> entry -> 'a -> 'a) -> t -> 'a -> 'a
  val empty : t
end

module Pp : sig
  open NLF
  val sign : Format.formatter -> sign -> unit
  val obj : Format.formatter -> obj -> unit
end

val lift : NLF.obj -> NLF.fam

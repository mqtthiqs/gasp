open Name

type variable = Name.variable
type constant = Name.constant

module rec NLF : sig
  type env = NLFEnv.t
  type sign = NLFSign.t

  type kind = 
    | KType of env			(* Γ type *)

  and fam = 
    | Fam of env * constant * env	     (* Γ ⊢ a Δ *)

  and obj =
    | Obj of env * head * env * constant * env     (* Γ ⊢ h Σ : a Δ *)

  and head =
    | HVar of variable
    | HConst of constant
    | HObj of obj
end

and NLFEnv : sig
  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj
  type t
  val add : t -> variable -> entry -> t
  val find : t -> variable -> entry
  val fold : (variable -> entry -> 'a -> 'a) -> t -> 'a -> 'a
  val merge : t -> t -> t
  val clear : t -> t
  val is_empty : t -> bool
  val empty : t
end

and NLFSign : sig
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
  type t
  val add : t -> constant -> entry -> t
  val find : t -> constant -> entry
  val fold : (constant -> entry -> 'a -> 'a) -> t -> 'a -> 'a
  val empty : t
end

val lift : NLF.obj -> NLF.fam

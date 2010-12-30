open Name

module rec NLF : sig
  type env = NLFEnv.t
  type sign = NLFSign.t

  type kind = 
    | Kind of env

  and fam = 
    | Fam of env * fhead

  and obj =
    | Obj of env * ohead * fhead

  and fhead =
    | FConst of constant * env

  and ohead =
    | OVar of variable * env
    | OConst of constant * env
    | OApp of obj * env
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

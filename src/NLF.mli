type variable = string
type constant = string

type name =
  | Name of variable
  | Anonymous

module rec NLF : sig
  type env = NLFEnv.t

  type kind = 
    | Kind of env
	
  and fam = 
    | Fam of env * fhead
	
  and obj =
    | Obj of env * ohead * fhead
	
  and fhead =
    | FVar of variable * env
    | FConst of constant * env
	
  and ohead =
    | OVar of variable * env
    | Oconst of constant * env
    | OApp of obj * env
end

and NLFEnv : sig
  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj
  type t
  val add : t -> name -> entry -> t
  val find : t -> name -> entry
  val empty : t
end

and NLFSign : sig
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
  type t
  val add : t -> constant -> entry -> t
  val find : t -> constant -> entry
  val empty : t
end

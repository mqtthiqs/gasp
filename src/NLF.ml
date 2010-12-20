open Name

module rec NLF : sig
  type env = NLFEnv.t

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
end = NLF

and NLFEnv : sig
  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj
  type t = (variable * entry) list
  val add : t -> variable -> entry -> t
  val find : t -> variable -> entry
  val empty : t
end = struct

  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj

  type t = (variable * entry) list

  let add env x e = (x,e)::env
  let find env x = List.assoc x env
  let empty = []
end


and NLFSign : sig
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
  type t = (constant * entry) list
  val add : t -> constant -> entry -> t
  val find : t -> constant -> entry
  val empty : t
end = struct
  
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
	
  type t = (constant * entry) list

  let add env x e = (x,e)::env
  let find env x = List.assoc x env
  let empty = []
end

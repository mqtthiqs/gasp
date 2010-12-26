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
end = NLF

and NLFEnv : sig
  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj
  type t = (variable * entry) list
  val add : t -> variable -> entry -> t
  val find : t -> variable -> entry
  val fold_decl : (variable -> NLF.fam -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_def : (variable -> NLF.obj -> 'a -> 'a) -> t -> 'a -> 'a
  val empty : t
end = struct

  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj

  type t = (variable * entry) list

  let add env x e = (x,e)::env
  let find env x = List.assoc x env
  let fold_decl f (e:t) acc = List.fold_left
    (fun acc (x,e) ->
       match e with
	 | ODef t -> acc
	 | ODecl a -> f x a acc
    ) acc e
  let fold_def f (e:t) acc = List.fold_left
    (fun acc (x,e) ->
       match e with
	 | ODef t -> f x t acc
	 | ODecl a -> acc
    ) acc e
  let empty = []
end

and NLFSign : sig
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
  type t = (constant * entry) list
  val add : t -> constant -> entry -> t
  val find : t -> constant -> entry
  val fold : (constant -> entry -> 'a -> 'a) -> t -> 'a -> 'a
  val empty : t
end = struct
  
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
	
  type t = (constant * entry) list

  let add env x e = (x,e)::env
  let find env x = List.assoc x env
  let fold f env acc = List.fold_left (fun acc (x,a) -> f x a acc) acc env
  let empty = []
end

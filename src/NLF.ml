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
  type t
  val add : t -> variable -> entry -> t
  val find : t -> variable -> entry
  val fold : (variable -> entry -> 'a -> 'a) -> t -> 'a -> 'a
  val merge : t -> t -> t
  val clear : t -> t
  val is_empty : t -> bool
  val empty : t
end = struct

  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj

  module Varmap = Map.Make(struct type t = variable let compare = Pervasives.compare end)

  type t = entry Varmap.t * variable list

  let add (m,a:t) x e = Varmap.add x e m, x::a
  let find (m,a:t) x = 
    (* try Varmap.find x m with Not_found -> failwith ("not found "^x) *)
    Varmap.find x m
  let fold f (m,a:t) acc = List.fold_left
    (fun acc x -> f x (Varmap.find x m) acc
    ) acc a
  let merge e1 e2 =
    fold
      (fun x e acc -> 
	 try 
	   match e, find acc x with
	     | ODecl _, ODef t -> add acc x (ODef t)
	     | ODecl _, ODecl a -> add acc x (ODecl a)
	     | _ -> assert false
	 with Not_found ->
	   add acc x e			(* TODO ajouter les dépendances *)
      ) e2 e1
  let clear(m,_) = m, []
  let is_empty (_, t) = t = []
  let empty = Varmap.empty, []
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
      
  let add env x e = env@[x,e]
  let find env x = List.assoc x env
  let fold f env acc = List.fold_left (fun acc (x,a) -> f x a acc) acc env
  let empty = []
end

module type Sig =
sig

  type variable

  type ('ty, 't) t
 
  val empty : unit -> ('ty, 't) t
    
  val define : variable -> 't -> 'ty -> ('ty, 't) t -> ('ty, 't) t
    
  val declare : variable -> 'ty -> ('ty, 't) t -> ('ty, 't) t
    
(*  exception Unbound of variable *)

  val lookup_declaration : variable -> ('ty, 't) t -> 'ty
    
(*  exception Undefined of variable *)
      
  val lookup_definition : variable -> ('ty, 't) t -> 't * 'ty

  val as_list : ('ty, 't) t -> (variable * 'ty * 't option) list

  val map : ('ty -> 'ty) -> ('t -> 't) -> ('ty, 't) t -> ('ty, 't) t

  val disjoint_join : ('ty, 't) t -> ('ty, 't) t -> ('ty, 't) t 

end

module Make (Name : sig type variable end) 
: Sig with type variable = Name.variable = 
struct

  open Name

  type variable = Name.variable
    
  type ('ty, 't) binding = 
    | Declare of 'ty
    | Define of 'ty * 't

  type ('ty, 't) t = (variable * ('ty, 't) binding) list

  let empty () = []

  let declare x ty e = (x, Declare ty) :: e

  let define x t ty e = (x, Define (ty, t)) :: e

  exception Unbound of Name.variable

  let lookup_declaration x e = 
    try
      match List.assoc x e with
	| Declare ty -> ty
	| Define (ty, _) -> ty
    with Not_found -> 
      raise (Unbound x)

  exception Undefined of Name.variable

  let lookup_definition x e = 
    try
      match List.assoc x e with
	| Declare _ -> raise (Undefined x)
	| Define (ty, t) -> (t, ty)
    with Not_found -> 
      raise (Unbound x)

  let as_list e = 
    List.rev 
      (List.fold_left (fun accu -> function
	| (x, Declare ty) -> (x, ty, None) :: accu
	| (x, Define (ty, t)) -> (x, ty, Some t) :: accu) [] 
	 e)

  let map on_type on_term = 
    List.map (function
      | (x, Declare ty) -> (x, Declare (on_type ty))
      | (x, Define (ty, t)) -> (x, Define (on_type ty, on_term t)))

  let disjoint_join e1 e2 = 
    (* FIXME: Check for disjointness. *)
    e1 @ e2

end

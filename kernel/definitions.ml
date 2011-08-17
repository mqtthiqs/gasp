module type Sig = 
sig

  include Environment.Sig 

  (* FIXME: Use phantom types to restrict to definitions only. *)
  type ('ty, 'o) definitions = ('ty, 'o) t
      
  type ('ty, 'o, 't) construct = 
    | Open   of variable * 't
    | Define of ('ty, 'o) definitions * 't

  val map_construct : ('ty -> 'ty) -> ('o -> 'o) -> ('t -> 't) 
    -> ('ty, 'o, 't) construct -> ('ty, 'o, 't) construct

  val ( --> ) : variable -> 'o -> 'ty -> ('ty, 'o) definitions

  val ( @@ ) : ('ty, 'o) definitions -> ('ty, 'o) definitions -> ('ty, 'o) definitions

end

module Make (Name : sig 
  type variable 
  val same_internal_names : variable -> variable -> bool
  val same_external_names : variable -> variable -> bool
end) : Sig with type variable = Name.variable = struct

  include Environment.Make (Name)

  (* FIXME: Use phantom types to restrict to definitions only. *)
  type ('ty, 'o) definitions = ('ty, 'o) t
      
  type ('ty, 'o, 't) construct = 
    | Open   of variable * 't
    | Define of ('ty, 'o) definitions * 't

  let map_construct on_type on_obj on_term = function
    | Open (x, t) -> Open (x, on_term t)
    | Define (defs, t) -> Define (map on_type on_obj defs, on_term t)
    
  let ( --> ) x o ty = 
    define x o ty (empty ())

  let ( @@ ) d d' = disjoint_join d d'

end

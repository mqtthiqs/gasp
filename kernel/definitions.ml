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

end

module Make (Name : sig type variable end) : Sig with type variable = Name.variable = struct

  include Environment.Make (Name)

  (* FIXME: Use phantom types to restrict to definitions only. *)
  type ('ty, 'o) definitions = ('ty, 'o) t
      
  type ('ty, 'o, 't) construct = 
    | Open   of variable * 't
    | Define of ('ty, 'o) definitions * 't

  let map_construct on_type on_obj on_term = function
    | Open (x, t) -> Open (x, on_term t)
    | Define (defs, t) -> Define (map on_type on_obj defs, on_term t)
    
end

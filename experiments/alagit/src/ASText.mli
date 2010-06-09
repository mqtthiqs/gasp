
(** Syntactic sugar. *)

(** [mk_arrow t1 t2] produces [t1 -> t2] using an
    anonymous dependent product. *)
val mk_arrow : AST.ptype' -> AST.ptype' -> AST.ptype

(** [is_arrow t] checks if [t] is an arrow, that is 
    an anonymous dependent product. *)
val is_arrow : AST.ptype -> bool


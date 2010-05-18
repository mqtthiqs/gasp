open AST

type t
type key = int				(* TODO retirer *)

val empty : t
val bind_def : t -> ptype -> key list -> key * t
val bind_decl : t -> id -> ptype -> key * t
val lookup : t -> key -> ptype

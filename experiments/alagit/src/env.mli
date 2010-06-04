open AST

type key

type head = 
  | Hsort of sort
  | Happ of head * key

type t

and j = t * head

val empty : t

val lookup : t -> key -> j

val bind_def : t -> key list -> j -> t * key

val bind_decl : t -> j -> t * key

exception Empty

val pop_decl : t -> t * key

val clear_decl : t -> t

open AST

type key

module Keymap : Map.S with type key = key
module Idmap : Map.S with type key = id

type head = 
  | Hsort of sort
  | Happ of head * key

type t

and j = { 
  env : t ;
  head : head;
  sort : sort
}

val empty : t

val lookup : t -> key -> j

val bind_def : t -> key list -> j -> t * key

val bind_decl : t -> j -> t * key

exception Empty

val pop_decl : t -> t * key

val clear_decl : t -> t

val keys_of : key Idmap.t -> term -> key list

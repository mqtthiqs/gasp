open AST
open Env

type t
val empty : t

val lookup : t -> id -> key
val bind : t -> id -> key -> t

(* Debug only *)
val fold : (id -> key -> 'a -> 'a) -> t -> 'a -> 'a

val keys_of : t -> term' -> key list

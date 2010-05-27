open AST

type t
type j = t * head

val empty : t

(* [bind_def env x a t] introduces the definition (x=a:t) in env. *)
val bind_def : t -> id -> term -> j -> t

(* [bind_decl env x t] introduces the declaration (x:t) in env *)
val bind_decl : t -> binder -> j -> t

(* [lookup env x] returns the type of x in env *)
val lookup : t -> id -> j

val link : t -> id -> id -> t

(* [equal env x y] finds out if x and y points to the same type *)
val equal : t -> t -> id -> id -> bool

exception Empty
val pop_decl : t -> t * id

val clear_decl : t -> t

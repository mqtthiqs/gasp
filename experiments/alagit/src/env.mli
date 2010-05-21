open AST

type t

val empty : t

(* [bind_def env x a t] introduces the definition (x=a:t) in env. *)
val bind_def : t -> id -> term -> ptype -> t

(* [bind_decl env x t] introduces the declaration (x:t) in env *)
val bind_decl : t -> id -> ptype -> t

(* [lookup env x] returns the type of x in env *)
val lookup : t -> id -> ptype

(* [lookup_and_bind env x y] returns the type of x in env, and binds y
   to the same type *)
val lookup_and_bind : t -> id -> id -> t * ptype

(* [equal env x y] finds out if x and y points to the same type *)
val equal : t -> id -> id -> bool

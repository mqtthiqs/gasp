open AST

type t

val empty : t

(* [bind_def env x a t] introduces the definition (x=a:t) in env. *)
val bind_def : t -> id -> term -> ptype -> t

(* [bind_decl env x t] introduces the declaration (x:t) in env *)
val bind_decl : t -> id -> ptype -> t

(* [lookup env x] returns the type of x in env *)
val lookup : t -> id -> ptype

(* [lookup_latest_with_prefix env x] returns the id of the latest entry that 
   has prefix [x]. *)
val lookup_latest_with_prefix : t -> string -> id

(* [lookup_and_bind env x y] returns the type of x in env, and binds y
   to the same type *)
val lookup_and_bind : t -> id -> id -> t * ptype

(* [equal env x y] finds out if x and y points to the same type *)
val equal : t -> id -> id -> bool

(* [to_ptype env] converts (back) an environment as a type. *)
val to_ptype : t -> ptype'

(* [expand env on_var on_app x] exports an internalized term using the
   two constructors [on_var] and [on_app]. *)
val expand : t -> (id -> 'a) -> ('a -> id -> 'a) -> id -> 'a

(* [subnames env x] returns the list of names of subterms of the term
   bound to [x]. *)
val subnames : t -> id -> id list

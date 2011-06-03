open Name
open NLF

type t

type entry =
  | FDecl of fconst * kind
  | ODecl of oconst * fam

val empty : t
val add_oconst : oconst -> fam -> t -> t
val add_fconst : fconst -> kind -> t -> t
val find_oconst : oconst -> t -> fam
val find_fconst : fconst -> t -> kind
val mem_oconst : oconst -> t -> bool
val mem_fconst : fconst -> t -> bool
val fold : (entry -> 'a -> 'a) -> t -> 'a -> 'a

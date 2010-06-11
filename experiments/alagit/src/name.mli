
(** Semi-internal names. *)
type t 

(** [to_string n] gives a pretty concrete representation for names. *)
val to_string : t -> string

(** [fresh prefix] generates a fresh name whose concrete representation 
    starts with prefix. The comparison function over fresh names depends
    on their introduction order. 
    If you have generated X then Y, X < Y otherwise Y > X. 
    This property is used in the type {!Env.t} to maintain the relative 
    order of bindings, even if we use a standard AVL tree based map
    internally. *)
(* FIXME: Maybe too risky. x*)
val fresh : string -> t

(** [has_prefix s n] returns true if [n] has been built initially using
    [s] as prefix. *)
val has_prefix : string -> t -> bool

(** [from_string s] generates a name whose concrete representation
    is exactly [s(0)]. No freshness guarantee provided. Besides, 
    there is no memorized rank of introduction. Therefore, if a 
    name from this function is used in an {!Env.t}, it is seen
    as globally defined. Therefore, this function should be used
    with extreme care. *)
val from_string : string -> t

(** [unique_from_string s] works as [from_string s] except that an
    extra check is performed to ensure that [s] is not the concrete
    representation of a previously generated name. *)
val unique_from_string : string -> t

(** [InternalNameAlreadyInUse] is raised when [unique_from_string] fails. *)
exception InternalNameAlreadyInUse

(** [compare n1 n2] is a standard library compliant comparison
    function. *)
val compare : t -> t -> int

(** [hash n] is standard library compliant hashing function. *)
val hash : t -> int

(** [to_string_debug n] gives a concrete representation for names. *)
val to_string_debug : t -> string

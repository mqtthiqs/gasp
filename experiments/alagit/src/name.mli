
(** Semi-internal names. *)
type t 

(** [to_string n] gives a concrete representation for names. *)
val to_string : t -> string

(** [fresh prefix] generates a fresh name whose concrete representation 
    starts with prefix. *)
val fresh : string -> t

(** [has_prefix s n] returns true if [n] has been built initially using
    [s] as prefix. *)
val has_prefix : string -> t -> bool

(** [from_string s] generates a name whose concrete representation
    is exactly [s(0)]. No freshness guarantee provided. *)
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

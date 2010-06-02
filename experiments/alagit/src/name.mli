
(** Semi-internal names. *)
type t 

(** [to_string n] gives a concrete representation for names. *)
val to_string : t -> string

(** [fresh prefix] generates a fresh name whose concrete representation 
    starts with prefix. *)
val fresh : string -> t

(** [from_string s] generates a name whose concrete representation
    is exactly [s(0)]. No freshness guarantee provided. *)
val from_string : string -> t

(** [compare n1 n2] is a standard library compliant comparison
    function. *)
val compare : t -> t -> int

(** [hash n] is standard library compliant hashing function. *)
val hash : t -> int

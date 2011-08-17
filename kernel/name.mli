type fconst
type oconst
type variable

val gen_status : unit -> int
val gen_init : int -> unit

val gen_variable : unit -> variable
val refresh_variable : variable -> variable

val same_external_names : variable -> variable -> bool
val same_internal_names : variable -> variable -> bool

val mk_variable : string -> variable
val mk_fconst : variable -> fconst
val mk_oconst : variable -> oconst
val of_variable : variable -> string
val of_fconst : fconst -> variable
val of_oconst : oconst -> variable

type head =
  | Var of variable
  | Cst of oconst

module Varmap : Map.S with type key = variable
module Varset : Set.S with type elt = variable
module Fconstmap : Map.S with type key = fconst
module Oconstmap : Map.S with type key = oconst

module Pp : sig
  open Print
  val variable : variable printing_fun
  val fconst : fconst printing_fun
  val oconst : oconst printing_fun
end

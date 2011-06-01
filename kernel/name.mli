type fconst
type oconst
type variable

val gen_status : unit -> int
val gen_init : int -> unit

val gen_variable : unit -> variable

val mk_variable : string -> variable
val mk_fconst : string -> fconst
val mk_oconst : string -> oconst
val of_variable : variable -> string
val of_fconst : fconst -> string
val of_oconst : oconst -> string

type name =
  | Named of variable
  | Anonymous

type head =
  | Var of variable
  | Cst of oconst

type position = (variable * int) option

val variable_for : name -> variable

module Varmap : Map.S with type key = variable
module Fconstmap : Map.S with type key = fconst
module Oconstmap : Map.S with type key = oconst

module Pp : sig
  open Print
  val variable : variable printing_fun
  val fconst : fconst printing_fun
  val oconst : oconst printing_fun
end

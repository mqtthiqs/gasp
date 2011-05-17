type constant
type variable

val gen_status : unit -> int
val gen_init : int -> unit

val gen_variable : unit -> variable

val mk_variable : string -> variable
val mk_constant : string -> constant
val of_variable : variable -> string
val of_constant : constant -> string

type name =
  | Named of variable
  | Anonymous

type position = (variable * int) option

val variable_for : name -> variable

module Varmap : Map.S with type key = variable
module Constmap : Map.S with type key = constant

module Pp : sig
  open Print
  val variable : variable printing_fun
  val constant : constant printing_fun
end

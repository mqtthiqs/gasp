type constant
type variable
type definition

val gen_status : unit -> int
val gen_init : int -> unit

val gen_variable : unit -> variable
val gen_constant : unit -> constant
val gen_definition : unit -> definition

val mk_variable : string -> variable
val mk_constant : string -> constant
val mk_definition : string -> definition
val of_variable : variable -> string
val of_constant : constant -> string
val of_definition : definition -> string

type path = variable list

type name =
  | Named of variable
  | Anonymous

val variable_for : name -> variable

module Varmap : Map.S with type key = variable
module Constmap : Map.S with type key = constant
module Defmap : Map.S with type key = definition

module Pp : sig
  open Print
  val variable : variable printing_fun
  val constant : constant printing_fun
  val definition : definition printing_fun
end

type constant = string
type variable = string

type name =
  | Named of variable
  | Anonymous

val gen_new : unit -> int
val gen_init : int -> unit
val gen_name : unit -> variable
val variable_for : name -> variable

module Varmap : Map.S with type key = variable
module Constmap : Map.S with type key = constant

type constant = string
type variable = string

type name =
  | Named of variable
  | Anonymous

val gen_name : unit -> variable
val name_of : name -> variable

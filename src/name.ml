type constant = string
type variable = string

type name =
  | Named of variable
  | Anonymous

let gen_name =
  let c = ref 0 in
  fun () ->
    incr c; "x" ^ (string_of_int !c)

let variable_for = function
  | Named x -> x
  | Anonymous -> gen_name()

module Varmap = Map.Make(struct type t = variable let compare = Pervasives.compare end)
module Constmap = Map.Make(struct type t = constant let compare = Pervasives.compare end)

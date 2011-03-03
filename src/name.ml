type constant = string
type variable = string

type name =
  | Named of variable
  | Anonymous

let gen_new, gen_init =
  let c = ref 0 in
  (fun () -> incr c; !c),
  (fun n -> c := n)

let gen_name () = "x" ^ (string_of_int (gen_new()))

let variable_for = function
  | Named x -> x
  | Anonymous -> gen_name()

module Varmap = Map.Make(struct type t = variable let compare = Pervasives.compare end)
module Constmap = Map.Make(struct type t = constant let compare = Pervasives.compare end)

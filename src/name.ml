include types of mli with

type variable = string and
type constant = string and
type definition = string and
module Varmap = Map.Make(struct type t = variable let compare = Pervasives.compare end) and 
module Constmap = Map.Make(struct type t = constant let compare = Pervasives.compare end) and
module Defmap = Map.Make(struct type t = definition let compare = Pervasives.compare end) and
module Pp = struct
  open Format
  let variable fmt x = fprintf fmt "%s" x
  let constant fmt x = fprintf fmt "%s" x
  let definition fmt x = fprintf fmt "$%s" x
end

let gen_new, gen_init, gen_status =
  let c = ref 0 in
  (fun () -> incr c; !c),
  (fun n -> c := n),
  (fun () -> !c)

let gen_variable () = "v" ^ (string_of_int (gen_new()))
let gen_constant () = "c" ^ (string_of_int (gen_new()))
let gen_definition () = string_of_int (gen_new())

let mk_variable s = s
let mk_constant s = s
let mk_definition s = s
let of_variable s = s
let of_constant s = s
let of_definition s = s

let variable_for = function
  | Named x -> x
  | Anonymous -> gen_variable()


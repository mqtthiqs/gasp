include types of mli with

type variable = string and
type fconst = string and
type oconst = string and
module Varmap = Map.Make(struct type t = variable let compare = Pervasives.compare end) and
module Fconstmap = Map.Make(struct type t = fconst let compare = Pervasives.compare end) and
module Oconstmap = Map.Make(struct type t = fconst let compare = Pervasives.compare end) and
module Pp = struct
  open Format
  let variable fmt x = fprintf fmt "%s" x
  let fconst fmt x = fprintf fmt "%s" x
  let oconst fmt x = fprintf fmt "%s" x
end

let gen_new, gen_init, gen_status =
  let c = ref 0 in
  (fun () -> incr c; !c),
  (fun n -> c := n),
  (fun () -> !c)

let gen_variable () = "v" ^ (string_of_int (gen_new()))
let gen_fconst () = "c" ^ (string_of_int (gen_new()))

let mk_variable s = s
let mk_fconst s = s
let mk_oconst s = s
let of_variable s = s
let of_fconst s = s
let of_oconst s = s

let variable_for = function
  | Named x -> x
  | Anonymous -> gen_variable()

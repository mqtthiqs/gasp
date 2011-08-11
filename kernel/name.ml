type variable = string
type fconst = string
type oconst = string
module Varset = Set.Make(struct type t = variable let compare = Pervasives.compare end)
module Varmap = Map.Make(struct type t = variable let compare = Pervasives.compare end)
module Fconstmap = Map.Make(struct type t = fconst let compare = Pervasives.compare end)
module Oconstmap = Map.Make(struct type t = fconst let compare = Pervasives.compare end)

type name = variable option

let equals_name x y = match x, y with
  | Some x, Some y -> x=y
  | _ -> false

type head =
  | Var of variable
  | Cst of oconst

type position = (variable * int) option

module Pp = struct
  open Format
  let variable fmt x = fprintf fmt "%s" x
  let name fmt = function
    | None -> fprintf fmt "_"
    | Some x -> variable fmt x

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

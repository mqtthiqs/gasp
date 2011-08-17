type variable = {
  external_name: string; (* This name is used for reference from scope outside. *)
  internal_name: int; (* This name is used internally and can be renamed.    *)
}

type fconst = string
type oconst = string

let compare_variable v1 v2 = compare v1 v2

let same_internal_names v1 v2 = v1.internal_name = v2.internal_name

let same_external_names v1 v2 = v1.external_name = v2.external_name

module Varset = Set.Make(struct type t = variable let compare = compare_variable end)
module Varmap = Map.Make(struct type t = variable let compare = compare_variable end)
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
  let variable fmt x = 
    if x.internal_name = 0 then 
      fprintf fmt "%s" x.external_name
    else 
      fprintf fmt "%s_%d" x.external_name x.internal_name

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

let gen_variable_name () = "v" ^ (string_of_int (gen_new()))

let gen_variable () = 
  let name = gen_variable_name () in {
    internal_name = gen_new ();
    external_name = name
  }

let refresh_variable x = {
  internal_name = gen_new ();
  external_name = x.external_name
}

let gen_fconst () = "c" ^ (string_of_int (gen_new()))

let mk_variable s = { 
  external_name = s;
  internal_name = 0;
}

let mk_fconst s = s.external_name
let mk_oconst s = s.external_name
let of_variable s = s.external_name ^ "_" ^ string_of_int s.internal_name
let of_fconst s = mk_variable s
let of_oconst s = mk_variable s

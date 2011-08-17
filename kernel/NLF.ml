open Name

type nlf_head =   
  | HConst of oconst
  | HVar of variable

module Body = LF.Make (struct
  type head_ = nlf_head
  type fhead_ = Name.fconst
end)

module Pp = LF_Pp.Make (struct
  include Body
  let pp_head fmt = function
    | HConst x -> Format.fprintf fmt "@[%s@]" (Name.of_oconst x)
    | HVar x -> Format.fprintf fmt "@[%s@]" (Name.of_variable x)
  let pp_fhead fmt x = 
    Format.fprintf fmt "@[%s@]" (Name.of_fconst x)
end)

module Utils = LF_utils.Make (Body)

module Refresh = Utils.Refresh (struct
  let head r = function
    | HVar x -> HVar (Utils.rename r x)
    | c -> c
  let fhead r c = c
end)

include Body

let head_as_obj h = 
  OApp (h, [])

(* FIXME: FConst, OVar and OConst should be forbidden in NLF because
   FIXME: they are represented as a @(h, []). 
   FIXME: We will use a stronger typing to do that. *)
let wf_obj = function
  | OConst _ | OVar _ -> false
  | _ -> true

let wf_fam = function
  | FConst _ -> false
  | _ -> true

let fconst x = 
  FApp (x, [])

let oconst x = 
  OApp (HConst x, [])

let destruct_fam_prod = function
  | FProd (x, a, b) -> 
    let (x, a, b) = Refresh.alpha_rename_prod x a b in
    Some (x, a, b)
  | _ -> None
    
let destruct_kind_prod = function
  | KProd (x, a, k) -> 
    let (x, a, k) = Refresh.alpha_rename_kind_prod x a k in
    Some (x, a, k)
  | _ -> None
    
let rec telescope_of_fam_prod a = 
  match destruct_fam_prod a with
    | None -> ([], a)
    | Some (x, a, b) -> 
      let (t, b) = telescope_of_fam_prod b in
      ((x, a) :: t, b)

let rec telescope_close t a = 
  match t with
    | [] -> a
    | (x, b) :: t -> FProd (x, b, telescope_close t a)

let rec telescope_of_kind_prod a = 
  match destruct_kind_prod a with
    | None -> ([], a)
    | Some (x, a, b) -> 
      let (t, b) = telescope_of_kind_prod b in 
      ((x, a) :: t, b)

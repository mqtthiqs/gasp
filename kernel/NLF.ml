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

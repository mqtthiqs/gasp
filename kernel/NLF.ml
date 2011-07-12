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
    
include Body

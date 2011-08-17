module rec ILF : sig
  include LF.Sig 
  type head_ 
  type fhead_ 
end 
with type head_ = ILF.obj 
and type head = ILF.head_ 
and type fhead_ = ILF.fam 
and type fhead = ILF.fhead_
= struct
  include LF.Make (ILF)
  type head_ = ILF.obj
  type fhead_ = ILF.fam
end

module rec Pp : sig
  include LF_Pp.Sig 
  with type obj = ILF.obj 
  and type head = ILF.obj
  and type fam = ILF.fam
  and type fhead = ILF.fam
  val pp_head : Format.formatter -> head -> unit
  val pp_fhead : Format.formatter -> fhead -> unit
end = LF_Pp.Make (struct
  include ILF
  let pp_head = Pp.pp_obj
  let pp_fhead = Pp.pp_fam 
end)
    
module Utils = LF_utils.Make (ILF)

let refresh_obj = ref None
module Refresh = Utils.Refresh (struct
  let head r = (Util.unSome !refresh_obj) r
  let fhead r c = c
end)
let _ = refresh_obj := Some (Refresh.obj)

module rec ILF : sig
  include LF.Sig 
  type t
end with type t = ILF.obj and type head = ILF.t = struct
  include LF.Make (ILF)
  type t = ILF.obj
end

module rec Pp : sig
  include LF_Pp.Sig with type obj = ILF.obj and type head = ILF.obj
  val pp_head : Format.formatter -> head -> unit
end = LF_Pp.Make (struct
  include ILF
  let pp_head : Format.formatter -> head -> unit = Pp.pp_obj
end)
    

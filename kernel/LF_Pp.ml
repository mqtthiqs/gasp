open Format

module type Sig = sig
  type obj
  type head
  val pp_obj : formatter -> obj -> unit
  val pp_head : formatter -> head -> unit
end

module Make (LF : sig 
  include LF.Sig 
  val pp_head : formatter -> head -> unit
end) 
: Sig with type obj = LF.obj and type head = LF.head = 
struct

  open LF
  module DefPP = Definitions_Pp.Make (Definitions)

  type obj = LF.obj
  type head = LF.head
  let pp_head = LF.pp_head

  let is_wilcard x = ((Name.of_variable x).[0] = '_')

  let rec pp_fam fmt = function
    | FConst f ->
      fprintf fmt "@[%s@]" (Name.of_fconst f)
    | FProd (x, a, b) when is_wilcard x -> 
      fprintf fmt "@[%a -> %a@]" pp_fam a pp_fam b
    | FProd (x, a, b) -> 
      fprintf fmt "@[{%a : %a} %a@]" pp_ident x pp_fam a pp_fam b      
    | FApp (f, args) -> 
      fprintf fmt "@[%a (%a)@]" pp_fam f pp_arguments args
    | FDef d ->
      DefPP.pp_construct pp_ident pp_opt_fam pp_obj pp_fam fmt d

  and pp_opt_fam fmt = function
    | None -> ()
    | Some x -> pp_fam fmt x

  and pp_arguments fmt = function
    | [] -> ()
    | h :: hs -> fprintf fmt "@[%a@,@[%a@]@]" pp_head h pp_arguments hs

  and pp_ident fmt x = 
    fprintf fmt "@[%s@]" (Name.of_variable x)

  and pp_obj fmt = function
    | OConst o -> 
      fprintf fmt "@[%s@]" (Name.of_oconst o)
    | OVar x -> 
      pp_ident fmt x
    | OLam (x, a, t) -> 
      fprintf fmt "@[[%a : %a]@,@[%a@]@]" pp_ident x pp_fam a pp_obj t
    | OApp (h, []) ->
      pp_head fmt h
    | OApp (h, args) -> 
      fprintf fmt "@[%a (%a)@]" pp_head h pp_arguments args
    | ODef d -> 
      DefPP.pp_construct pp_ident pp_opt_fam pp_obj pp_obj fmt d

  and pp_kind fmt = function
    | KType -> 
      fprintf fmt "type"
    | KProd (x, a, k) -> 
      fprintf fmt "{%a : %a} %a" pp_ident x pp_fam a pp_kind k

end

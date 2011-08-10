open Format

module type Sig = sig
  type obj
  type fam
  type head
  type fhead 
  type kind
  val pp_obj : formatter -> obj -> unit
  val pp_fam : formatter -> fam -> unit
  val pp_head : formatter -> head -> unit
  val pp_fhead : formatter -> fhead -> unit
  val pp_kind : formatter -> kind -> unit
end

module Make (LF : sig 
  include LF.Sig 
  val pp_head : formatter -> head -> unit
  val pp_fhead : formatter -> fhead -> unit
end) 
: Sig
  with type obj = LF.obj 
  and type fam = LF.fam
  and type head = LF.head 
  and type fhead = LF.fhead
  and type kind = LF.kind
=  struct

  open LF
  module DefPP = Definitions_Pp.Make (Definitions)

  type obj = LF.obj
  type fam = LF.fam
  type head = LF.head
  type fhead = LF.fhead
  type kind = LF.kind 

  let pp_head = LF.pp_head
  let pp_fhead = LF.pp_fhead

  let is_wilcard x = ((Name.of_variable x).[0] = '_')

  let rec pp_fam fmt = function
    | FConst f ->
      fprintf fmt "@[%s@]" (Name.of_fconst f)
    | FProd (x, a, b) when is_wilcard x -> 
      fprintf fmt "@[%a -> %a@]" pp_fam a pp_fam b
    | FProd (x, a, b) -> 
      fprintf fmt "@[{%a : %a} %a@]" pp_ident x pp_fam a pp_fam b      
    | FApp (f, args) -> 
      fprintf fmt "@[%a (%a)@]" pp_fhead f pp_arguments args
    | FDef d ->
      DefPP.pp_construct pp_ident pp_opt_fam pp_obj pp_fam fmt d

  and pp_opt_fam fmt = function
    | None -> fprintf fmt "@[?@]"
    | Some x -> pp_fam fmt x

  and pp_arguments fmt = function
    | [] -> ()
    | [ h ] -> fprintf fmt "@[%a@]" pp_head h 
    | h :: hs -> fprintf fmt "@[%a@;@[%a@]@]" pp_head h pp_arguments hs

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
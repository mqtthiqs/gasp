open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FHead of subst * fconst * args

and obj =
  | Obj of subst * value
  | OBox of obj * position * obj

and args = value list

and ohead = XLF.ohead

and value =
  | VHead of ohead
  | VLam of variable * obj

and def = ohead * args

and subst = (variable * def) list

module Pp : sig
  open Print
  val obj : obj printing_fun
  val fam : fam printing_fun
  val kind : kind printing_fun
end

module Check : sig
  open NLF
  (* val obj : NLF.obj -> obj -> unit *)
  (* val arg : NLF.obj -> value -> NLF.fam -> unit *)
  (* val fam : NLF.obj -> fam -> unit *)
  (* val kind : NLF.obj -> kind -> unit *)
end

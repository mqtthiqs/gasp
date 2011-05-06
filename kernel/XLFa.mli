open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of constant * args * kind

and obj =
  | OLam of variable * fam * obj
  | OHead of ohead * args * fam
  | OMeta of variable * fam
  | OBox of obj * variable * (variable * obj)

and ohead = XLF.ohead

and args = (variable * obj) list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list

module Pp : sig
  open Format
  val sign : formatter -> sign -> unit
  val obj : formatter -> obj -> unit
  val fam : formatter -> fam -> unit
  val kind : formatter -> kind -> unit
  val args : formatter -> args -> unit
end


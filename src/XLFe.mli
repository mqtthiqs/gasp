open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fhead =
  | FConst of constant * args

and fam =
  | FProd of variable * fam * fam
  | FHead of fhead

and ohead =
  | OVar of variable * args * fhead
  | OMeta of NLF.variable * args * fhead
  | OConst of constant * args * fhead
  | OApp of obj * args * fhead

and obj =
  | OLam of variable * fam * obj
  | OHead of ohead

and args = (variable * obj) list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list

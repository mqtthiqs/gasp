open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of constant * args

and obj =
  | OLam of variable * fam * obj
  | OHead of ohead * args
  | OMeta of definition
  | OBox of obj * variable * subst

and ohead = 
  | HVar of variable
  | HConst of constant

and subst = (variable * obj) list

and args = obj list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list

open Name

type fam' =
  | FConst of constant
  | FProd of name * fam * fam
  | FApp of fam * obj

and fam = fam' Position.located

and obj' = 
  | OMeta of NLF.variable
  | OConst of constant
  | OVar of variable
  | OLam of name * fam * obj
  | OApp of obj * obj

and obj = obj' Position.located

type kind' =
  | KType
  | KProd of name * fam * kind

and kind = kind' Position.located

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list

type entity =
  | Kind of kind
  | Fam of fam
  | Obj of obj

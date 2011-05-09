open Name

type fam =
  | FConst of constant
  | FProd of name * fam * fam
  | FApp of fam * obj

and obj = 
  | OConst of constant
  | OVar of variable
  | OLam of name * fam * obj
  | OApp of obj * obj
  | OBox of obj * position * subst

and subst = variable * obj

type kind =
  | KType
  | KProd of name * fam * kind

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list

type entity =
  | Kind of kind
  | Fam of fam
  | Obj of obj

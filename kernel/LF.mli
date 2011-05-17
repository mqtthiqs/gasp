open Name

type fam =
  | FConst of fconst
  | FProd of name * fam * fam
  | FApp of fam * obj

and obj = 
  | OConst of oconst
  | OVar of variable
  | OLam of name * obj
  | OApp of obj * obj
  | OBox of obj * position * obj

type kind =
  | KType
  | KProd of name * fam * kind

type entry =
  | FDecl of kind
  | ODecl of fam

type env = (variable * fam) list

type entity =
  | Kind of kind
  | Fam of fam
  | Obj of obj

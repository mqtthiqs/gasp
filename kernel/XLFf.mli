open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FHead of subst * constant * args

and obj =
  | Obj of subst * value
  | OBox of obj * position * obj

and args = value list

and vhead = XLF.ohead

and value =
  | VHead of vhead
  | VLam of variable * obj

and def = vhead * args

and subst = (variable * def) list

type entry =
  | FDecl of kind
  | ODecl of fam

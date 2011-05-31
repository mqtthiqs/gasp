open Name

type fam =
  | FAtom of fatom
  | FProd of variable * fam * fam

and fatom = subst * fconst * args

and obj =
  | Obj of subst * value

and args = value list

and ohead = XLF.ohead

and value =
  | VHead of ohead * fatom
  | VLam of variable * fam * obj

and def =
  | DAtom of ohead * args * fatom           (* ohead bindé ds repo *)
  | DHead of ohead * fam		    (* ohead bindé ds env *)

and subst = def Varmap.t

type kind =
  | KProd of variable * fam * kind
  | KType

type entry =
  | FDecl of fconst * kind
  | ODecl of oconst * fam

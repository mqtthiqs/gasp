open Name

type fam =
  | FAtom of fatom
  | FProd of variable * fam * fam

and fatom = value Varmap.t * subst * fconst * args

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

module Pp : sig
  open Print
  val obj : obj printing_fun
end

val go : obj -> position -> value
val bind : variable -> def -> obj -> obj
val lift_def : variable -> obj -> args * fam

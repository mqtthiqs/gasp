open Name

type fam = XLF.fam

and obj =
  | Obj of subst * value

and args = value Varmap.t

and ohead = XLF.ohead

and value =
  | VHead of ohead * fconst * XLF.args
  | VLam of variable * fam * obj

and def =
  | DApp of ohead * args * fconst * XLF.args (* ohead bindé ds repo *)
  | DHead of ohead * fam		    (* ohead bindé ds env *)

and subst = def Varmap.t

module Pp : sig
  open Print
  val obj : obj printing_fun
end

val go : obj -> position -> def -> obj
val lift_def : variable -> obj -> fam
val to_def : obj -> def
val bidon : obj
val bidon_type : fam

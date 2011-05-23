open Name

module rec NLF : sig

  type fam = XLF.fam

  and obj =
    | Obj of subst * value

  and args = value list

  and ohead = XLF.ohead

  and value =
    | VHead of ohead * fconst * XLF.args
    | VLam of variable * fam * obj

  and def =
    | DApp of ohead * args * fconst * XLF.args (* ohead bindé ds repo *)
    | DHead of ohead * fam		    (* ohead bindé ds env *)

  and subst = def NLFSubst.t
end

and NLFSubst : (Map.S with type key = variable)

module Pp : sig
  open Print
  val obj : NLF.obj printing_fun
end

val go : NLF.obj -> position -> NLF.def -> NLF.obj
val lift_def : variable -> NLF.obj -> NLF.fam
val to_def : NLF.obj -> NLF.def
val bidon : NLF.obj
val bidon_type : NLF.fam

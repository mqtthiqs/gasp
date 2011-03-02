open NLF

val obj : NLFSubst.t -> NLFSign.t -> XLF.obj -> XLFa.obj
val fam : NLFSubst.t -> NLFSign.t -> XLF.fam -> XLFa.fam
val kind : NLFSubst.t -> NLFSign.t -> XLF.kind -> XLFa.kind
val entry : (NLFSign.t -> XLFa.entry -> NLF.entry) -> 
  NLFSign.t -> XLF.entry -> NLF.entry 

val from_obj : XLFa.obj -> XLF.obj
val from_fam : XLFa.fam -> XLF.fam
val from_kind : XLFa.kind -> XLF.kind
val from_sign : XLFa.sign -> XLF.sign


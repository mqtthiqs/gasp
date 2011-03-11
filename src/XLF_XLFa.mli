open NLF

val obj : NLFSign.t -> NLF.obj -> XLF.obj -> XLFa.obj
val fam : NLFSign.t -> NLF.obj -> XLF.fam -> XLFa.fam
val kind : NLFSign.t -> NLF.obj -> XLF.kind -> XLFa.kind
val entry : (NLFSign.t -> XLFa.entry -> NLF.entry) -> 
  NLFSign.t -> XLF.entry -> NLF.entry 

val from_obj : XLFa.obj -> XLF.obj
val from_fam : XLFa.fam -> XLF.fam
val from_kind : XLFa.kind -> XLF.kind
val from_sign : XLFa.sign -> XLF.sign


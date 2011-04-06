open NLF

val obj : XLFe.obj -> XLFn.obj
val fam : XLFe.fam -> XLFn.fam
val kind : XLFe.kind -> XLFn.kind
val entry : (NLFSign.t -> XLFn.entry -> NLF.entry) -> 
  NLFSign.t -> XLFe.entry -> NLF.entry 

val from_obj : XLFn.obj -> XLFe.obj
val from_fam : XLFn.fam -> XLFe.fam
val from_kind : XLFn.kind -> XLFe.kind
val from_sign : XLFn.sign -> XLFe.sign

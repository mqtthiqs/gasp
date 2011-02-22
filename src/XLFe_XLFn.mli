open NLF

val obj : XLFe.obj -> XLFn.obj
val fam : XLFe.fam -> XLFn.fam
val kind : XLFe.kind -> XLFn.kind
val entry : (NLFSign.t -> XLFn.entry -> NLFSign.entry) -> 
  NLFSign.t -> XLFe.entry -> NLFSign.entry 

open NLF

val obj : XLFa.obj -> XLFe.obj
val fam : XLFa.fam -> XLFe.fam
val kind : XLFa.kind -> XLFe.kind
val entry : (NLF.sign -> XLFe.entry -> NLFSign.entry) -> 
  NLFSign.t -> XLFa.entry -> NLFSign.entry 

val from_obj : XLFe.obj -> XLFa.obj
val from_fam : XLFe.fam -> XLFa.fam
val from_kind : XLFe.kind -> XLFa.kind
val from_sign : XLFe.sign -> XLFa.sign
val from_args : XLFe.args -> XLFa.args

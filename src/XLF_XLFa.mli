open NLF

val obj : NLFEnv.t -> NLFSign.t -> XLF.obj -> XLFa.obj
val fam : NLFEnv.t -> NLFSign.t -> XLF.fam -> XLFa.fam
val kind : NLFEnv.t -> NLFSign.t -> XLF.kind -> XLFa.kind
val entry : (NLF.sign -> XLFa.entry -> NLFSign.entry) -> 
  NLFSign.t -> XLF.entry -> NLFSign.entry 

val from_obj : XLFa.obj -> XLF.obj
val from_sign : XLFa.sign -> XLF.sign

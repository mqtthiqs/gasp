open NLF

val obj : NLFEnv.t -> NLFSign.t -> XLF.obj -> XLFa.obj
val fam : NLFEnv.t -> XLFa.sign -> XLFa.env -> XLF.fam -> XLFa.fam
val kind : NLFEnv.t -> XLFa.sign -> XLFa.env -> XLF.kind -> XLFa.kind
val sign : XLFa.sign -> XLF.sign -> XLFa.sign

val from_obj : XLFa.obj -> XLF.obj
val from_sign : XLFa.sign -> XLF.sign

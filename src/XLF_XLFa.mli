val obj : NLF.NLFEnv.t -> XLFa.sign -> XLFa.env -> XLF.obj -> XLFa.obj
val fam : NLF.NLFEnv.t -> XLFa.sign -> XLFa.env -> XLF.fam -> XLFa.fam
val kind : NLF.NLFEnv.t -> XLFa.sign -> XLFa.env -> XLF.kind -> XLFa.kind
val sign : NLF.NLFEnv.t -> XLFa.sign -> XLF.sign -> XLFa.sign

val from_obj : XLFa.obj -> XLF.obj
val from_sign : XLFa.sign -> XLF.sign

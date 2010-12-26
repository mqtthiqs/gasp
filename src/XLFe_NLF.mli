open Name
open NLF

val obj : NLFEnv.t -> XLFe.obj -> NLF.obj
val fam : NLFEnv.t -> XLFe.fam -> NLF.fam
val kind : NLFEnv.t -> XLFe.kind -> NLF.kind
val sign : NLFSign.t -> XLFe.sign -> NLFSign.t

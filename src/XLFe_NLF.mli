open Name
open NLF

val obj : NLF.env -> XLFe.obj -> NLF.obj
val fam : NLF.env -> XLFe.fam -> NLF.fam
val kind : NLF.env -> XLFe.kind -> NLF.kind
val entry : (NLF.sign -> NLFSign.entry -> NLFSign.entry) -> 
  NLFSign.t -> XLFe.entry -> NLFSign.entry 

val from_env_args : NLF.env -> XLFe.args
val from_env_kind : NLF.env -> XLFe.kind -> XLFe.kind
val from_sign : NLF.sign -> XLFe.sign
val from_kind : NLF.kind -> XLFe.kind
val from_fam : NLF.fam -> XLFe.fam
val from_obj : NLF.obj -> XLFe.obj

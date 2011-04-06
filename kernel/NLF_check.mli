exception Not_convertible_fam of NLF.NLF.fam * NLF.NLF.fam
exception Not_convertible_obj of NLF.NLF.obj * NLF.NLF.obj

val conv_args : NLF.NLF.env -> NLF.NLF.env -> unit
val conv_obj : NLF.NLF.obj -> NLF.NLF.obj -> unit
val conv_fam : NLF.NLF.fam -> NLF.NLF.fam -> unit
val args : NLF.NLFSign.t -> NLF.NLF.env -> NLF.NLF.env -> NLF.NLF.env -> unit
val obj : NLF.NLFSign.t -> NLF.NLF.obj -> unit
val fam : NLF.NLFSign.t -> NLF.NLF.fam -> unit
val kind : NLF.NLFSign.t -> NLF.NLF.kind -> unit
val sign : NLF.NLFSign.t -> unit

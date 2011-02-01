type t

val init : SLF.sign -> t
val compile : t -> SLF.term -> t

val load : unit -> t
val save : t -> unit

open Struct

val init : SLF.sign -> Repo.t
val commit : Repo.t -> Env.t -> SLF.term -> Repo.t
val checkout : Repo.t -> SLF.term

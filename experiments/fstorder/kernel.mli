open LF
open Names
open Struct

exception Not_conv_obj of repo * env * obj * obj
exception Not_conv_fam of repo * env * fam * fam
exception Non_functional_fapp of repo * env * spine
exception Non_functional_app of repo * env * spine * fam
exception Non_functional_obj of repo * env * obj * fam
exception Unbound_meta of repo * Meta.t
exception Not_evaluable of repo * SLF.term

val push : repo -> env -> head * spine -> repo
val pull : repo -> Meta.t * subst -> obj
val init : repo -> SLF.sign -> repo

(****** debugging only ******)
module Conv : sig
  val obj : repo -> env -> obj * obj * fam -> unit
  val fam : repo -> env -> fam * fam -> unit
end
module Check : sig
  val obj : red:bool -> repo -> env -> obj * fam -> repo * obj
end

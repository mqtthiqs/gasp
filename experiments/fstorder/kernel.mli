val push : Repo.t -> LF.Env.t -> LF.head * LF.spine -> Repo.t * int
val pull : Repo.t -> Names.Meta.t -> LF.obj
val init : LF.Sign.t -> SLF.sign -> LF.Sign.t

module Conv : sig
  exception Not_conv_obj of Repo.t * LF.obj * LF.obj
  exception Not_conv_fam of Repo.t * LF.fam * LF.fam
  (* debugging only *)
  val obj : Repo.t -> LF.obj * LF.obj -> unit
end

(* debugging only *)
module Check : sig
  val obj : Repo.t -> LF.Env.t -> LF.obj * LF.fam -> Repo.t * LF.obj
end

val push : Repo.t -> LF.Env.t -> LF.head * LF.spine -> Repo.t
val pull : Repo.t -> Names.Meta.t -> LF.obj
val init : LF.Sign.t -> SLF.sign -> LF.Sign.t


(* debugging only *)
module Check : sig

  val obj : Repo.t -> LF.Env.t -> LF.obj * LF.fam -> Repo.t * LF.obj

end

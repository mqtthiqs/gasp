open AST

val infer_env  : Env.t -> ptype' -> Env.t

val infer_type : Env.t -> ptype' -> sort

open AST

val infer_type : Env.t -> term' -> Env.j * sort

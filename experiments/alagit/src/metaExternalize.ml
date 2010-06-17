(** Externalization never produces meta-level AST. *)
let on env f (x, y) = MetaAST.Object (f x y)

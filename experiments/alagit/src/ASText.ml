let noname_prefix = "*"

let noname () = Name.fresh noname_prefix

let mk_arrow t1 t2 = AST.Prod (noname (), t1, t2)

let is_arrow = function
  | AST.Prod (x, _, _) -> Name.has_prefix noname_prefix x
  | _ -> false


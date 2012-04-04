open Camlp4.PreCast

EXTEND Gram Syntax.expr:
  [ [ "HERE" ->
    <:expr< Camlp4.PreCast.($Ast.Meta.MetaLoc.meta_loc_expr _loc _loc$) >>
  ] ];
END

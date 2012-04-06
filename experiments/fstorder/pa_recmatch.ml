open Camlp4.PreCast
open Camlp4.PreCast.Syntax

EXTEND Gram expr: LEVEL "top"
  [ [ "match"; e1 = expr; "rec"; e2 = expr; "with"; a = match_case ->
    <:expr<
      let c0 = $e2$ in
      let rec __f = fun
        [$a$
        | default ->
            let v =
              try c0 default
              with e ->
                let open Camlp4.PreCast in
                let loc = $Ast.Meta.MetaLoc.meta_loc_expr _loc _loc$ in
                raise (Util.Located (loc, e))
            in __f v
        ] in
      __f $e1$
    >>
    ] ];
END

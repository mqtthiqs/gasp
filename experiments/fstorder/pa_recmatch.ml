open Camlp4.PreCast
open Camlp4.PreCast.Syntax

EXTEND Gram
expr: LEVEL "top" [
  [ "match"; "*"; e1 = sequence; o = OPT ["in"; e2 = sequence -> e2]; "with"; a = match_case ->
  let e2 = match o with None -> <:expr< [] >> | Some e2 -> e2 in
  <:expr<
    let c0 = eval $e2$ in
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

  | "let"; "*"; p = patt; o = OPT ["in"; e2 = expr LEVEL "apply" -> e2]; "=";
    e1 = sequence; "in"; e3 = sequence ->
    let e2 = match o with None -> <:expr< [] >> | Some e2 -> e2 in
  <:expr<
      let c0 = eval $e2$ in
      let rec __f = fun
        [$p$ -> $e3$
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
  ]
];
END

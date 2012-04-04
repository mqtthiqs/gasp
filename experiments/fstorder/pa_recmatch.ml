open Camlp4.PreCast
open Camlp4.PreCast.Syntax

EXTEND Gram expr: LEVEL "top"
  [ [ "match"; e1 = expr; "rec"; e2 = expr; "with"; a = match_case ->
    <:expr<
      let c0 = $e2$ in
      let rec __f = fun [$a$ | default -> __f (c0 default) ] in __f $e1$
    >>
    ] ];
END












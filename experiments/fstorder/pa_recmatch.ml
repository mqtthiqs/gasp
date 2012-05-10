open Camlp4.PreCast
open Camlp4.PreCast.Syntax

EXTEND Gram
expr: LEVEL "top" [
  [ "match"; "*"; e1 = sequence; o = OPT ["in"; e2 = sequence -> e2]; "with"; a = match_case ->
  let e2 = match o with None -> <:expr< [] >> | Some e2 -> e2 in
  <:expr<
    let (__repo, __v) = __eval __repo $e2$ $e1$ in
    match __v with
      [$a$
      | default -> failwith "match failure"]
  >>

  | "let"; "*"; p = patt; o = OPT ["in"; e2 = expr LEVEL "apply" -> e2]; "=";
    e1 = sequence; "in"; e3 = sequence ->
    let e2 = match o with None -> <:expr< [] >> | Some e2 -> e2 in
    <:expr<
      let (__repo, __v) = __eval __repo $e2$ $e1$ in
      match __v with
          [ $p$ -> $e3$
          | t -> raise (Kernel.Not_evaluable (__repo, t)) ]
  >>

  | "return"; e=expr -> <:expr< (__repo, $e$) >>
  ]
];
END

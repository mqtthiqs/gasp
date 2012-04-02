
module ExprParser = struct

  open Camlp4.PreCast

  let ident = Gram.Entry.mk "ident"
  let binder = Gram.Entry.mk "binder"
  let subst = Gram.Entry.mk "subst"
  let term = Gram.Entry.mk "term"
  let term1 = Gram.Entry.mk "term1"
  let term2 = Gram.Entry.mk "term2"
  let term_eoi = Gram.Entry.mk "term_eoi"
  let env = Gram.Entry.mk "env"
  let env_eoi = Gram.Entry.mk "env_eoi"

  EXTEND Gram

  ident:
  [ [ x = LIDENT -> x
    | x = UIDENT -> x ]
  ];

  binder:
  [ [ x = ident -> <:expr< Some $str:x$ >>
    | "_" -> <:expr< None >>
    | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_expr _loc s
    ]
  ];

  env:
  [ [ -> <:expr< [] >>
    | e = env; ";"; x = binder; ":"; t = term -> <:expr< [($x$, $t$) :: $e$] >>
    | x = binder; ":"; t = term -> <:expr< [($x$, $t$)] >>
    | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_expr _loc s
    ]
  ];

  env_eoi:
      [[ e = env; `EOI -> e ]];

  term:
  [ "prd" RIGHTA
      [ "type" -> <:expr< SLF.Type >>
      | "{"; id = binder; ":"; t = term; "}"; u = term ->
      <:expr< SLF.Prod($id$, $t$, $u$) >> ]
  | "arr" RIGHTA
      [ t = term; "->"; u = term -> <:expr< SLF.Prod(None, $t$, $u$) >> ]
  | "rarr" RIGHTA
      [ t = term; "<-"; u = term -> <:expr< SLF.Prod(None, $u$, $t$) >> ]
  | "term1"
      [ t = term1 -> t ]
  ];

  term1:
  [ "app" LEFTA
      [ t = term1; u = term2 -> <:expr<  SLF.App ($t$, $u$) >> ]
  | "term2"
      [ t = term2 -> t ]
  ];

  term2:
  [ "simple"
      [ x = ident -> <:expr< SLF.Ident $str:x$ >>
      | "?"; x = ident -> <:expr< SLF.Meta ($str:x$, []) >>
      | "?"; x = ident; "["; s = subst; "]" -> <:expr< SLF.Meta ($str:x$, $s$) >>
      | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_expr _loc s
      | "("; t = term; ")" -> t ]
  | "lam"
      [ "["; id = binder; "]"; t = term1 ->
      <:expr< SLF.Lam ($id$, $t$) >> ]
  ];

  term_eoi:
      [[ t = term; `EOI -> t ]];

  subst:
  [ [ -> <:expr< [] >>
    | t = term -> <:expr< [$t$] >>
    | s = subst; ";"; t = term -> <:expr< [$t$ :: $s$] >> ]
  ];

  END;;

  let sign = Gram.Entry.mk "sign"
  let sign_eoi = Gram.Entry.mk "sign_eoi"

  let rec build_patt = function
    | [] -> <:patt@here< [] >>
    | x :: xs -> <:patt@here< [$lid:x$ :: $build_patt xs$] >>

  let rec build_app f = function
    | x :: xs -> <:expr@here< $build_app f xs$ $lid:x$ >>
    | [] -> f

  let rec fun_telescope i = function
    | <:expr@_loc< SLF.Prod ($_$, $_$, $xs$) >> ->
      ("__xxx"^string_of_int i) :: fun_telescope (succ i) xs
    | _ -> []

  EXTEND Gram

  sign:
  [[ -> <:expr< [] >>
   | x = ident; ":"; t = term; "="; e = term; "."; s = sign ->
     let names = fun_telescope 0 t in
     <:expr<
             let $lid:x$ = fun (repo:Struct.repo) (env:Struct.env) ->
             let rec $lid:x$ = $e$ in
             fun
               [ $build_patt names$ -> $build_app <:expr<$lid:x$>> (List.rev names)$
               | _ -> assert False ]
             in [($str:x$, $t$, SLF.Defined $lid:x$) :: $s$]
     >>
   | x = ident; ":"; t = term; "."; s = sign ->
     <:expr< [($str:x$, $t$, SLF.Sliceable) :: $s$] >>
   | "#"; x = ident; ":"; t = term; "."; s = sign ->
     <:expr< [($str:x$, $t$, SLF.Non_sliceable) :: $s$] >>
   | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_expr _loc s
   ]];
  sign_eoi: [[ s = sign; `EOI -> <:expr< ($s$ : SLF.sign) >>]];
  END;;

end

module PattParser = struct

  open Camlp4.PreCast

  let ident = Gram.Entry.mk "ident"
  let binder = Gram.Entry.mk "binder"
  let subst = Gram.Entry.mk "subst"
  let term = Gram.Entry.mk "term"
  let term1 = Gram.Entry.mk "term1"
  let term2 = Gram.Entry.mk "term2"
  let term_eoi = Gram.Entry.mk "term_eoi"

  EXTEND Gram

  ident:
  [ [ x = LIDENT -> <:patt< $str:x$ >>
    | x = UIDENT -> <:patt< $str:x$ >>
    | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_patt _loc s
    ]
  ];

  binder:
  [ [ x = ident -> <:patt< Some $x$ >>
    | "_" -> <:patt< None >>
    | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_patt _loc s
    ]
  ];

  term:
  [ "prd" RIGHTA
      [ "type" -> <:patt< SLF.Type >>
      | "{"; id = binder; ":"; t = term; "}"; u = term ->
      <:patt< SLF.Prod($id$, $t$, $u$) >> ]
  | "arr" RIGHTA
      [ t = term; "->"; u = term -> <:patt< SLF.Prod(None, $t$, $u$) >> ]
  | "rarr" RIGHTA
      [ t = term; "<-"; u = term -> <:patt< SLF.Prod(None, $u$, $t$) >> ]
  | "term1"
      [ t = term1 -> t ]
  ];

  term1:
  [ "app" LEFTA
      [ t = term1; u = term2 -> <:patt<  SLF.App ($t$, $u$) >> ]
  | "term2"
      [ t = term2 -> t ]
  ];

  term2:
  [ "simple"
      [ x = ident -> <:patt< SLF.Ident $x$ >>
      | "?"; x = ident -> <:patt< SLF.Meta ($x$, _) >>
      | "?"; x = ident; "["; s = subst; "]" -> <:patt< SLF.Meta ($x$, $s$) >>
      | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_patt _loc s
      | "("; t = term; ")" -> t ]
  | "lam"
      [ "["; id = binder; "]"; t = term1 ->
      <:patt< SLF.Lam ($id$, $t$) >> ]
  ];

  term_eoi:
      [[ t = term; `EOI -> t ]];

  subst:
  [ [ -> <:patt< [] >>
    | t = term -> <:patt< [$t$] >>
    | s = subst; ";"; t = term -> <:patt< [$t$ :: $s$] >>
    | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_patt _loc s
    ]
  ];

  END;;

end


module Quotations = struct

  open Camlp4.PreCast
  open Camlp4.PreCast.Syntax.Quotation

  let expand_expr_term loc _ s =
    let q = !Camlp4_config.antiquotations in
    Camlp4_config.antiquotations := true;
    let res = Gram.parse_string ExprParser.term_eoi loc s in
    Camlp4_config.antiquotations := q;
    res

  let expand_env_term loc _ s =
    let q = !Camlp4_config.antiquotations in
    Camlp4_config.antiquotations := true;
    let res = Gram.parse_string ExprParser.env_eoi loc s in
    Camlp4_config.antiquotations := q;
    res

  let expand_patt_term loc _ s =
    let q = !Camlp4_config.antiquotations in
    Camlp4_config.antiquotations := true;
    let res = Gram.parse_string PattParser.term_eoi loc s in
    Camlp4_config.antiquotations := q;
    res

  let expand_expr_sign loc _ s =
    let q = !Camlp4_config.antiquotations in
    Camlp4_config.antiquotations := true;
    let res = Gram.parse_string ExprParser.sign_eoi loc s in
    Camlp4_config.antiquotations := q;
    res

  let _ =
    add "env" DynAst.expr_tag expand_env_term;
    add "term" DynAst.patt_tag expand_patt_term;
    add "term" DynAst.expr_tag expand_expr_term;
    add "sign" DynAst.expr_tag expand_expr_sign;
    default := "term";

end

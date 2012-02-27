type term =
  | Type
  | Prod of string option * term * term
  | Lam of string option * term
  | App of term * term
  | Ident of string
  | Meta of string * term list

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of (term list -> term)

type sign = (string * term * entry_type) list

module ExprParser = struct

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
  [ [ x = LIDENT -> x
    | x = UIDENT -> x ]
  ];

  binder:
  [ [ x = ident -> <:expr< Some $str:x$ >>
    | "_" -> <:expr< None >> ]
  ];

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

  let rec fun_telescope err l e = function
    | <:expr< SLF.Prod (Some $str:x$, $_$, $xs$) >> -> fun_telescope err (x :: l) e xs
    | <:expr< Prod (None, $_$, $xs$) >> -> fun_telescope err ("__bla" :: l) e xs
    | _ -> <:expr@here< fun [ $build_patt (List.rev l)$ -> $e$ | _ -> failwith ("Match failure: "^ $str:err$ ) ] >>

  EXTEND Gram

  sign:
  [[ -> <:expr< [] >>
   | x = ident; ":"; t = term; "="; e = term; "."; s = sign ->
     <:expr< let rec $lid:x$ = $fun_telescope x [] e t$
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
  [ [ x = LIDENT -> x
    | x = UIDENT -> x ]
  ];

  binder:
  [ [ x = ident -> <:patt< Some $str:x$ >>
    | "_" -> <:patt< None >> ]
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
      [ x = ident -> <:patt< SLF.Ident $str:x$ >>
      | "?"; x = ident -> <:patt< SLF.Meta (x, []) >>
      | "?"; x = ident; "["; s = subst; "]" -> <:patt< SLF.Meta (x, $s$) >>
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
    add "term" DynAst.patt_tag expand_patt_term;
    add "term" DynAst.expr_tag expand_expr_term;
    add "sign" DynAst.expr_tag expand_expr_sign;
    default := "term";

end

module Printer = struct

  open Print
  open Format

  let str fmt s = fprintf fmt "%s" s

  let term_prec = function
    | Type | Ident _ | Meta _ -> 0
    | Lam _ -> 20
    | App _ -> 10
    | Prod _ -> 30

  let term pp fmt = function
    | Meta (x, []) -> fprintf fmt "?%s" x
    | Meta (x, s) -> fprintf fmt "?%s[%a]" x (pr_list pr_comma (pp (<=))) (List.rev s)
    | Ident x -> str fmt x
    | Prod (Some x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]"
	str x (pp (<)) a (pp (<=)) b
    | Lam (Some x, t) -> fprintf fmt "@[[%s]@ %a@]" x (pp (<=)) t
    | Lam (None, t) -> fprintf fmt "@[[_]@ %a@]" (pp (<=)) t
    | Prod (None, a, b) -> fprintf fmt "@[%a@ ->@ %a@]" (pp (<)) a (pp (<=)) b
    | App (t,u) -> fprintf fmt "@[%a@ %a@]" (pp (<=)) t (pp (<)) u
    | Type -> fprintf fmt "@[type@]"
      
  let term fmt t = pr_paren term term_prec 100 (<=) fmt t

  let sharp b fmt x = if b then fprintf fmt "@[#%a@]" str x else fprintf fmt "@[%a@]" str x
  let code fmt f = fprintf fmt "<fun>"

  let rec sign fmt = function
    | [] -> ()
    | (x, t, Sliceable) :: s -> fprintf fmt "@[%a : %a.@]@,%a" str x term t sign s
    | (x, t, Non_sliceable) :: s -> fprintf fmt "@[#%a : %a.@]@,%a" str x term t sign s
    | (x, t, Defined f) :: s -> fprintf fmt "@[%a : %a = %a@].@,%a" str x term t code f sign s
  let sign fmt s = fprintf fmt "@,@[<v>%a@]" sign s
end

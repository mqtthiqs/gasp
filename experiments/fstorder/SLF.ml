type term =
  | Type
  | Prod of string option * term * term
  | Lam of string option * term
  | App of term * term
  | Ident of string
  | Meta of string * term list

type sign = (string * term * bool * (term list -> term) option) list

module Parser = struct

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
  let ident_sharp = Gram.Entry.mk "ident_sharp"
  let expr_opt = Gram.Entry.mk "expr_opt"

  EXTEND Gram
  ident_sharp:
  [[
    x = ident -> x, <:expr< True >>
  | "#"; x = ident -> x, <:expr< False >>
  ]];

  expr_opt:
  [[
    -> <:expr< None >>
  | e = term -> <:expr< Some e >>
  ]];

  sign:
  [[ -> <:expr< [] >>
   | (x, b) = ident_sharp; ":"; t = term; "="; e = expr_opt; "."; s = sign ->
     <:expr< [($str:x$, $t$, $b$, $e$) :: $s$] >>
   | `ANTIQUOT ("", s) -> Syntax.AntiquotSyntax.parse_expr _loc s
   ]];
  sign_eoi: [[ s = sign; `EOI -> s]];
  END;;

  open Syntax.Quotation

  let expand_term_quot loc _ s =
    let q = !Camlp4_config.antiquotations in
    Camlp4_config.antiquotations := true;
    let res = Gram.parse_string term_eoi loc s in
    Camlp4_config.antiquotations := q;
    res

  let expand_sign_quot loc _ s =
    let q = !Camlp4_config.antiquotations in
    Camlp4_config.antiquotations := true;
    let res = Gram.parse_string sign_eoi loc s in
    Camlp4_config.antiquotations := q;
    res

  let _ =
    add "term" DynAst.expr_tag expand_term_quot;
    add "sign" DynAst.expr_tag expand_sign_quot;
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
  let code fmt f = fprintf fmt "..."

  let rec sign fmt = function
    | [] -> ()
    | (x, t, b, Some f) :: s -> fprintf fmt "@[%a : %a = @[%a@]@].@,%a" (sharp b) x term t sign s code f
    | (x, t, b, None) :: s -> fprintf fmt "@[%a : %a@].@,%a" (sharp b) x term t sign s
  let sign fmt s = fprintf fmt "@,@[<v>%a@]" sign s
end

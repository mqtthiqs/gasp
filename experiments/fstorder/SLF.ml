type term =
  | Type
  | Prod of string option * term * term
  | App of term * term
  | Ident of string
  | Meta of string

type sign =
  | Nil
  | Cons of string * term * sign

module Parser = struct

  open Camlp4.PreCast

  let ident = Gram.Entry.mk "ident"
  let term = Gram.Entry.mk "term"
  let term_eoi = Gram.Entry.mk "term_eoi"

  EXTEND Gram

  ident:
    [ [ x = LIDENT -> x
      | x = UIDENT -> x ]];

  term :
  [ "prd" RIGHTA
      [ "type" -> <:expr< SLF.Type >>
      | "{"; id = ident; ":"; t = term; "}"; u = term ->
      <:expr< SLF.Prod(Some($str:id$), $t$, $u$) >> ]
  | "arr" RIGHTA
      [ t = term; "->"; u = term -> <:expr< SLF.Prod(None, $t$, $u$) >> ]
  | "rarr" RIGHTA
      [ t = term; "<-"; u = term -> <:expr< SLF.Prod(None, $u$, $t$) >> ]
  | "app" LEFTA
      [ t = term; u = term -> <:expr<  SLF.App ($t$, $u$) >> ]
  | "simple"
      [ x = ident -> <:expr< SLF.Ident $str:x$ >>
      | "$"; x = ident -> <:expr< SLF.Meta $str:x$ >>
      | "("; t = term; ")" -> t ]
  ];

  term_eoi:
      [[ t = term; `EOI -> t ]];

  END;;

  let sign = Gram.Entry.mk "sign"
  let sign_eoi = Gram.Entry.mk "sign_eoi"

  EXTEND Gram
  sign:
  [[ -> <:expr< SLF.Nil >>
   | x = LIDENT; ":"; t = term; "."; s = sign -> <:expr< SLF.Cons ($str:x$, $t$, $s$) >>
   ]];
  sign_eoi: [[ s = sign; `EOI -> s]];
  END;;

  module Quot = struct

    open Syntax.Quotation

    let expand_term_quot loc _ s = Gram.parse_string term_eoi loc s
    let expand_sign_quot loc _ s = Gram.parse_string sign_eoi loc s

    let _ =
      add "raw_term" DynAst.expr_tag expand_term_quot;
      add "raw_sign" DynAst.expr_tag expand_sign_quot;
  end

end

module Printer = struct

  open Print
  open Format

  let str fmt s = fprintf fmt "%s" s

  let term_prec = function
    | Type | Ident _ | Meta _ -> 0
    | App _ -> 10
    | Prod _ -> 30

  let term pp fmt = function
    | Meta x | Ident x -> str fmt x
    | Prod (Some x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]"
	str x (pp (<=)) a (pp (<=)) b
    | Prod (None, a, b) -> fprintf fmt "@[%a@ ->@ %a@]" (pp (<=)) a (pp (<=)) b
    | App (t,u) -> fprintf fmt "@[%a@ %a@]" (pp (<=)) t (pp (<)) u
    | Type -> fprintf fmt "@[type@]"
      
  let term fmt t = pr_paren term term_prec 100 (<=) fmt t

  let rec sign fmt = function
    | Nil -> ()
    | Cons (x, t, s) -> fprintf fmt "@[%a : %a@].@,%a" str x term t sign s
  let sign fmt s = fprintf fmt "@,@[<v>%a@]" sign s
end

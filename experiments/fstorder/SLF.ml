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

module rec Strat : sig

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  val term : LF.Sign.t -> string option list -> term -> entity
  val obj : LF.Sign.t -> string option list -> term -> LF.obj
  val fam : LF.Sign.t -> string option list -> term -> LF.fam
  val kind : LF.Sign.t -> string option list -> term -> LF.kind
  val entry_type : LF.Sign.t -> entry_type -> LF.entry_type
end = struct

  open Util
  open Names

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  let rec app sign env l = function
    | Ident x ->
      begin
        try let i = List.index (Some x) env in
            Obj (LF.inj @@ LF.OApp (LF.HVar i, l))
        with Not_found ->
          try let x = OConst.make x in
              ignore (LF.Sign.ofind x sign);
              Obj (LF.inj @@ LF.OApp (LF.HConst x, l))
          with Not_found ->
            let x = FConst.make x in
            try ignore(LF.Sign.ffind x sign); Fam (LF.FApp (x, l))
            with Not_found -> failwith ("strat: not found "^FConst.repr x)
      end
    | App (t, u) ->
      begin match term sign env u with
        | Obj u ->  app sign env (u :: l) t
        | _ -> failwith "strat: argument is not an object"
      end
    | _ -> failwith "strat: app error"

  and term sign env = function
    | Lam (x, t) -> Obj (LF.inj @@ LF.OLam (x, obj sign (x :: env) t))
    | Type -> Kind LF.KType
    | Prod (x, a, b) ->
      begin match term sign env a, term sign (x::env) b with
        | Fam a, Kind k -> Kind (LF.KProd (x, a, k))
        | Fam a, Fam b -> Fam (LF.FProd (x, a, b))
        | Kind _, _ -> failwith "strat: prod argument is a kind"
        | Fam _, Obj _ -> failwith "strat: prod body is an obj"
        | Obj _, _ -> failwith "strat: prod argument is an obj"
      end
    | App _ as a -> app sign env [] a
    | Ident x ->
      begin try Obj (LF.inj @@ LF.OApp (LF.HVar (List.index (Some x) env), []))
        with Not_found ->
          try let x = OConst.make x in
              ignore(LF.Sign.ofind x sign); Obj (LF.inj @@ LF.OApp (LF.HConst x, []))
          with Not_found ->
            let x = FConst.make x in
            try ignore(LF.Sign.ffind x sign); Fam (LF.FApp (x, []))
            with Not_found -> failwith ("strat: not found "^FConst.repr x)
      end
    | Meta (x, s) ->
      let s = List.map (obj sign env) s in
      Obj (LF.inj @@ LF.OMeta (Meta.make x, s))

  and obj sign env t = match term sign env t with
    | Obj m -> m
    | _ -> failwith "strat: not an obj"

  let fam sign env t = match term sign env t with
    | Fam a -> a
    | _ -> failwith "strat: not a fam"

  let kind sign env t = match term sign env t with
    | Kind k -> k
    | _ -> failwith "strat: not a kind"

  let fn s (f : term list -> term) (l : LF.obj list) : LF.obj =
    let l = List.map (Unstrat.obj []) l in
    obj s [] (f l)

  let entry_type s = function
    | Sliceable -> LF.Sliceable
    | Non_sliceable -> LF.Non_sliceable
    | Defined f -> LF.Defined (fn s f)

end

and Unstrat : sig
  open LF
  val obj : string option list -> obj -> term
  val fam : string option list -> fam -> term
  val kind : string option list -> kind -> term
  val fn : Sign.t -> (obj list -> obj) -> term list -> term
  val sign : Sign.t -> sign
end = struct

  open Util

  let rec obj env = LF.prj @> function
    | LF.OLam (x, m) -> Lam (x, obj (x :: env) m)
    | LF.OApp (h, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (head env h)) l
    | LF.OMeta (x, s) -> Meta (Names.Meta.repr x, List.map (obj env) s)

  and head env = function
    | LF.HConst c -> Names.OConst.repr c
    | LF.HVar x ->
      try match List.nth env x with
        | Some x -> x
        | None -> "___"^(string_of_int x)
      with Failure "nth" -> "_REL_"^(string_of_int x)

  let rec fam env = function
    | LF.FApp (f, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (Names.FConst.repr f)) l
    | LF.FProd (x, a, b) -> Prod (x, fam env a, fam (x :: env) b)

  let rec kind env = function
    | LF.KType -> Type
    | LF.KProd (x, a, b) -> Prod (x, fam env a, kind (x :: env) b)

  let fn s (f : LF.obj list -> LF.obj) (l : term list) : term =
    let l = List.map (Strat.obj s []) l in
    obj [] (f l)

  let entry_type s = function
    | LF.Sliceable -> Sliceable
    | LF.Non_sliceable -> Non_sliceable
    | LF.Defined f -> Defined (fn s f)

  let sign (s : LF.Sign.t) =
    LF.Sign.fold
      (fun x (a, e) l -> (Names.OConst.repr x, fam [] a, entry_type s e) :: l)
      (fun x k l -> (Names.FConst.repr x, kind [] k, Sliceable) :: l)
      s []
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


  let eobj e fmt m = term fmt (Unstrat.obj e m)
  let efam e fmt a = term fmt (Unstrat.fam e a)
  let ekind e fmt k = term fmt (Unstrat.kind e k)

  let obj fmt m = eobj [] fmt m
  let fam fmt m = efam [] fmt m
  let kind fmt m = ekind [] fmt m

  let entity fmt = function
    | Strat.Kind k -> kind fmt k
    | Strat.Fam a -> fam fmt a
    | Strat.Obj m -> obj fmt m

  let sign fmt (s : LF.Sign.t) =
    let l = Unstrat.sign s in
    sign fmt l

  let env fmt e =
    let open Format in
    let var fmt = function
      | Some x -> fprintf fmt "%s" x
      | None -> fprintf fmt "_" in
    let rec aux (l:LF.Env.t) fmt = function
      | [] -> ()
      | [x,a] -> fprintf fmt "@[%a@ :@ %a@]" var x (efam (LF.Env.names_of l)) a
      | (x,a) :: e -> fprintf fmt "%a,@ %a" (aux l) [x, a] (aux (LF.Env.add x a l)) e
    in
    Format.fprintf fmt "@[%a@]" (aux LF.Env.empty) (List.rev (LF.Env.to_list e))

end

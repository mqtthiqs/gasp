open Names
open Struct

type binder = string option

type term =
  | Type
  | Prod of binder * term * term
  | Lam of binder * term
  | App of term * term
  | Ident of string
  | Meta of string * term list

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of (repo -> term list -> term)

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
             let $lid:x$ = fun (repo:Struct.repo) ->
             let rec $lid:x$ = $e$ in
             fun
               [ $build_patt names$ -> $build_app <:expr<$lid:x$>> names$
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

module rec Strat : sig

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  val term : Struct.sign -> binder list -> term -> entity
  val obj : Struct.sign -> binder list -> term -> LF.obj
  val fam : Struct.sign -> binder list -> term -> LF.fam
  val kind : Struct.sign -> binder list -> term -> LF.kind
  val entry_type : entry_type -> Sign.entry_type
  val env : Struct.sign -> (binder * term) list -> env
end = struct

  open Util
  open Names
  open Struct

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  let lookup sign env x l =
    try let i = List.index (Some x) env in
        Obj (LF.inj @@ LF.OApp (LF.HVar i, l))
    with Not_found ->
      try let x = OConst.make x in
          ignore (Sign.ofind x sign);
          Obj (LF.inj @@ LF.OApp (LF.HConst x, l))
      with Not_found ->
        let x = FConst.make x in
        try ignore(Sign.ffind x sign); Fam (LF.FApp (x, l))
        with Not_found -> failwith ("strat: not found "^FConst.repr x)

  let rec app sign env l = function
    | Ident x -> lookup sign env x l
    | App (t, u) -> app sign env (obj sign env u :: l) t
    | t -> Obj (LF.Subst.spine (obj sign env t) l)

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
    | App (t, u) -> app sign env [obj sign env u] t
    | Ident x -> lookup sign env x []
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

  let fn (f : repo -> term list -> term) repo (l : LF.obj list) : LF.obj =
    let l = List.map (Unstrat.obj []) l in
    obj repo.Repo.sign [] (f repo l)

  let entry_type = function
    | Sliceable -> Sign.Sliceable
    | Non_sliceable -> Sign.Non_sliceable
    | Defined f -> Sign.Defined (fn f)

  let rec env sign = function
    | [] -> Env.empty
    | (x, t) :: e ->
      let e = env sign e in
      Env.add x (fam sign (Env.names_of e) t) e

end

and Unstrat : sig
  open LF
  val obj : binder list -> obj -> term
  val fam : binder list -> fam -> term
  val kind : binder list -> kind -> term
  val sign : Struct.sign -> sign
end = struct

  open Util

  let rec obj env = LF.prj @> function
    | LF.OLam (x, m) -> Lam (x, obj (x :: env) m)
    | LF.OApp (h, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (head env h)) l
    | LF.OMeta (x, s) -> Meta (Meta.repr x, List.map (obj env) s)

  and head env = function
    | LF.HConst c -> OConst.repr c
    | LF.HVar x ->
      try match List.nth env x with
        | Some x -> x
        | None -> "___"^(string_of_int x)
      with Failure "nth" -> "_REL_"^(string_of_int x)

  let rec fam env = function
    | LF.FApp (f, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (FConst.repr f)) l
    | LF.FProd (x, a, b) -> Prod (x, fam env a, fam (x :: env) b)

  let rec kind env = function
    | LF.KType -> Type
    | LF.KProd (x, a, b) -> Prod (x, fam env a, kind (x :: env) b)

  let fn s (f : repo -> LF.obj list -> LF.obj) repo (l : term list) : term =
    let l = List.map (Strat.obj s []) l in
    obj [] (f repo l)

  let entry_type s = function
    | Sign.Sliceable -> Sliceable
    | Sign.Non_sliceable -> Non_sliceable
    | Sign.Defined f -> Defined (fn s f)

  let sign (s : Struct.sign) =
    Sign.fold
      (fun x (a, e) l -> (OConst.repr x, fam [] a, entry_type s e) :: l)
      (fun x k l -> (FConst.repr x, kind [] k, Sliceable) :: l)
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

  let sign fmt (s : Struct.sign) =
    let l = Unstrat.sign s in
    sign fmt l

  let env fmt e =
    let open Format in
    let var fmt = function
      | Some x -> fprintf fmt "%s" x
      | None -> fprintf fmt "_" in
    let rec aux (l:env) fmt = function
      | [] -> ()
      | [x,a] -> fprintf fmt "@[%a@ :@ %a@]" var x (efam (Env.names_of l)) a
      | (x,a) :: e -> fprintf fmt "%a,@ %a" (aux l) [x, a] (aux (Env.add x a l)) e
    in
    Format.fprintf fmt "@[%a@]" (aux Env.empty) (List.rev (Env.to_list e))

    let context fmt c =
      fprintf fmt "@,@[<v>";
      Context.fold
        (fun x (e, m, a) () ->
          let e' = Env.names_of e in
          Format.fprintf fmt "%a[%a] : @[%a@] = @[%a@]@,"
            Meta.print x env e (efam e') a (eobj e') m
        ) c ();
      fprintf fmt "@]"

    let repo_light fmt {Repo.sign; Repo.ctx; Repo.head} =
      Format.fprintf fmt "%a ⊢ %a@,"
        context ctx
        Meta.print head

    let repo fmt {Repo.sign = s; Repo.ctx; Repo.head} =
      Format.fprintf fmt "Signature:@ %a@,Context:@ %a@ ⊢ %a@,"
        sign s context ctx Meta.print head
end

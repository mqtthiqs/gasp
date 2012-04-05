open Util
open Names
open Struct

type binder = string option

type ident =
  | Id of string
  | Unnamed of int
  | Unbound of int

type term =
  | Type
  | Prod of binder * term * term
  | Lam of binder * term
  | App of term * term
  | Ident of ident
  | Meta of string * term list

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of (repo -> env -> term list -> term)

type sign = (string * term * entry_type) list

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
  val env : Struct.sign -> Struct.env -> (binder * term) list -> env
end = struct

  open Util
  open Names
  open Struct

  type entity =
    | Kind of LF.kind
    | Fam of LF.fam
    | Obj of LF.obj

  let lookup sign env x l =
    try let i = List.index ((=) (Some x)) env in
        Obj (LF.mkApp (LF.HVar i, l))
    with Not_found ->
      try let x = OConst.make x in
          ignore (Sign.ofind x sign);
          Obj (LF.mkApp (LF.HConst x, l))
      with Not_found ->
        let x = FConst.make x in
        try ignore(Sign.ffind x sign); Fam (LF.FApp (x, l))
        with Not_found -> failwith ("strat: not found "^FConst.repr x)

  let rec app sign env l = function
    | Ident (Id x) -> lookup sign env x l
    | Ident (Unnamed n) -> failwith ("strat: unnamed variable "^(string_of_int n))
    | Ident (Unbound n) -> failwith ("strat: unbound variable "^(string_of_int n))
    | App (t, u) -> app sign env (obj sign env u :: l) t
    | t -> Obj (LF.Subst.spine (obj sign env t) l)

  and term sign env = function
    | Lam (x, t) -> Obj (LF.mkLam (x, obj sign (x :: env) t))
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
    | Ident (Id x) -> lookup sign env x []
    | Ident (Unnamed n) -> failwith ("strat: unnamed variable "^(string_of_int n))
    | Ident (Unbound n) -> failwith ("strat: unbound variable "^(string_of_int n))
    | Meta (x, s) ->
      let s = List.map (obj sign env) s in
      Obj (LF.mkMeta (Meta.make x, s))

  and obj sign env t = match term sign env t with
    | Obj m -> m
    | _ -> failwith "strat: not an obj"

  let fam sign env t = match term sign env t with
    | Fam a -> a
    | _ -> failwith "strat: not a fam"

  let kind sign env t = match term sign env t with
    | Kind k -> k
    | _ -> failwith "strat: not a kind"

  let fn (f : repo -> env -> term list -> term) repo env (l : LF.obj list) : LF.obj =
    let l = List.map (Unstrat.obj (Env.names_of env)) l in
    obj repo.Repo.sign (Env.names_of env) (f repo env l)

  let entry_type = function
    | Sliceable -> Sign.Sliceable
    | Non_sliceable -> Sign.Non_sliceable
    | Defined f -> Sign.Defined (fn f)

  let rec env sign e0 = function
    | [] -> e0
    | (x, t) :: e ->
      let e = env sign e0 e in
      (x, fam sign (Env.names_of e) t) :: e

end

and Unstrat : sig
  open LF
  val obj : binder list -> obj -> term
  val fam : binder list -> fam -> term
  val kind : binder list -> kind -> term
  val env : env -> (binder * term) list
  val sign : Struct.sign -> sign
end = struct

  open Util

  let bound_env (env:Struct.env) x =
    List.memp (function _, (None, _) -> false | x, (Some y, _) -> x=y) x env

  let bound_names (env:binder list) x =
    List.memp (function _, None -> false | x, Some y -> x=y) x env

  let rec fresh bound = function
    | None -> None
    | Some x ->
        if bound x
        then fresh bound (Some (x^"'"))
        else Some x

  let head env = function
    | LF.HConst c -> Id (OConst.repr c)
    | LF.HVar x ->
      try match List.nth env x with
        | Some s -> Id s
        | None -> Unnamed x
      with Failure "nth" -> Unbound x

  let rec obj env = LF.prj @> function
    | LF.OLam (x, m) ->
        let x = fresh (bound_names env) x in
        Lam (x, obj (x :: env) m)
    | LF.OApp (h, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (head env h)) l
    | LF.OMeta (x, s) -> Meta (Meta.repr x, List.map (obj env) s)

  let rec fam env = function
    | LF.FApp (f, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (Id (FConst.repr f))) l
    | LF.FProd (x, a, b) ->
        let x = fresh (bound_names env) x in
        Prod (x, fam env a, fam (x :: env) b)

  let rec kind env = function
    | LF.KType -> Type
    | LF.KProd (x, a, b) ->
        let x = fresh (bound_names env) x in
        Prod (x, fam env a, kind (x :: env) b)

  let fn s (f : repo -> env -> LF.obj list -> LF.obj) repo env (l : term list) : term =
    let l = List.map (Strat.obj s (Env.names_of env)) l in
    obj [] (f repo env l)

  let entry_type s = function
    | Sign.Sliceable -> Sliceable
    | Sign.Non_sliceable -> Non_sliceable
    | Sign.Defined f -> Defined (fn s f)

  let sign (s : Struct.sign) =
    Sign.fold
      (fun x (a, e) l -> (OConst.repr x, fam [] a, entry_type s e) :: l)
      (fun x k l -> (FConst.repr x, kind [] k, Sliceable) :: l)
      s []

  let env (e : env) : (binder * term) list =
    let rec aux = function
      | (x, a) :: e ->
        let x = fresh (bound_env e) x in
        (x, fam (List.map fst e) a) :: aux e
      | [] -> []
    in aux e
end

module Printer = struct

  open Format
  open Print

  let str fmt s = fprintf fmt "%s" s

  let term_prec = function
    | Type | Ident _ | Meta _ -> 0
    | Lam _ -> 20
    | App _ -> 10
    | Prod _ -> 30

  let ident fmt = function
    | Id s -> str fmt s
    | Unnamed n -> fprintf fmt "_UNNAMED_%d" n
    | Unbound n -> fprintf fmt "_UNBOUND_%d" n

  let term pp fmt = function
    | Meta (x, []) -> fprintf fmt "?%s" x
    | Meta (x, s) -> fprintf fmt "?%s[@[%a@]]" x (list_rev semi (pp (fun _ _ -> true))) s
    | Ident x -> ident fmt x
    | Prod (None, a, b) -> fprintf fmt "@[<hov 2>%a@ ->@ %a@]" (pp (<)) a (pp (<=)) b
    | Prod (Some x,a,b) -> fprintf fmt "@[<hov 2>@[<h>{%a@ :@ %a}@]@ %a@]"
	str x (pp (<=)) a (pp (<=)) b
    | Lam (Some x, t) -> fprintf fmt "@[<hov 2>[%s]@ %a@]" x (pp (<=)) t
    | Lam (None, t) -> fprintf fmt "@[<hov 2>[_]@ %a@]" (pp (<=)) t
    | App (t,u) -> fprintf fmt "@[<hov 2>%a@ %a@]" (pp (<=)) t (pp (<)) u
    | Type -> fprintf fmt "@[type@]"
      
  let term fmt t = paren term term_prec 100 (<=) fmt t

  let sharp b fmt x = if b then fprintf fmt "@[#%a@]" str x else fprintf fmt "@[%a@]" str x
  let code fmt f = fprintf fmt "<fun>"

  let rec sign fmt = function
    | [] -> ()
    | [x, t, Sliceable] -> fprintf fmt "@[%a : %a.@]" str x term t
    | [x, t, Non_sliceable] -> fprintf fmt "@[#%a : %a.@]" str x term t
    | [x, t, Defined _] -> fprintf fmt "@[%a@ :@ %a@ =@ <fun>.@]" str x term t
    | a :: s -> fprintf fmt "%a@,%a" sign [a] sign s

  let sign fmt s = fprintf fmt "@,@[<v>%a@]" sign s

  let eobj e fmt m = term fmt (Unstrat.obj e m)
  let efam e fmt a = term fmt (Unstrat.fam e a)
  let ekind e fmt k = term fmt (Unstrat.kind e k)

  let obj fmt m = eobj [] fmt m
  let fam fmt m = efam [] fmt m
  let kind fmt m = ekind [] fmt m

  let esubst e fmt s = fprintf fmt "@[[%a]@]" (list_rev semi (eobj e)) s
  let subst = esubst []

  let entity fmt = function
    | Strat.Kind k -> kind fmt k
    | Strat.Fam a -> fam fmt a
    | Strat.Obj m -> obj fmt m

  let sign fmt (s : Struct.sign) =
    let l = Unstrat.sign s in
    sign fmt l

  let binder fmt = function
    | Some x -> fprintf fmt "%s" x
    | None -> fprintf fmt "_"

  let senv fmt e =
    let rec aux fmt = function
      | [] -> ()
      | [x,a] -> fprintf fmt "@[%a@ :@ %a@]" binder x term a
      | (x,a) :: e -> fprintf fmt "%a,@ %a" aux e aux [x, a]
    in
    Format.fprintf fmt "@[%a@]" aux e

  let env fmt e = senv fmt (Unstrat.env e)

  let context fmt c =
    fprintf fmt "@[<v>";
    Context.fold
      (fun x (e, m, a) () ->
        let e' = Env.names_of e in
        Format.fprintf fmt "@[<hov 2>%a[%a]@ :@ @[%a@] = %a@].@,"
          Meta.print x env e (efam e') a (eobj e') m
      ) c ();
    fprintf fmt "@]"

    let repo_light fmt {Repo.sign; Repo.ctx; Repo.head} =
      Format.fprintf fmt
        "{@ @[<v>@[<hov 2>ctx@ =@ %a@];@ @[<hov 2>head = %a%a@]@]@ }"
        context ctx
        Meta.print (fst head) subst (snd head)

    let repo fmt {Repo.sign = s; Repo.ctx; Repo.head} =
      Format.fprintf fmt
        "{@ @[<v>@[<hov 2>sign@ =@ %a@];@ @[<hov 2>ctx@ =@ %a@];@ @[<hov 2>head = %a%a@]@]@ }"
        sign s context ctx Meta.print (fst head) subst (snd head)
end

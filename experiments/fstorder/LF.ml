open Util
open Names
open Esubst

type head =
  | HVar of int
  | HConst of OConst.t

type fam =
  | FApp of FConst.t * obj list
  | FProd of string option * fam * fam

and obj =
  | XLam of string option * obj
  | XApp of head * spine
  | XMeta of Meta.t * subst
  | XClos of obj subs * obj

and spine = obj list
and subst = obj list

type kind =
  | KType
  | KProd of string option * fam * kind

type cobj =
  | OLam of string option * obj
  | OApp of head * spine
  | OMeta of Meta.t * subst

let inj = function
  | OLam (x, t) -> XLam (x, t)
  | OApp (h, l) -> XApp (h, l)
  | OMeta (x, s) -> XMeta (x, s)

module ESubst = struct

  let rec clos = function
    | s, m when is_subs_id s -> m
    | s, XClos (s', m) -> XClos (comp clos s s', m)
    | s, m -> XClos (s, m)

  let rec obj s = function
    | XClos (s', m) -> obj (comp clos s s') m
    | XMeta (x, l) -> OMeta (x, List.map (fun m -> clos (s, m)) l)
    | XLam (x, m) -> OLam (x, clos (subs_lift s, m))
    | XApp (HConst c, l) -> OApp (HConst c, List.map (fun m -> clos (s, m)) l)
    | XApp (HVar n, l) -> match expand_rel (n+1) s with
        | Inl (k, m) -> spine (obj (subs_shft (k, subs_id 0)) m, l)
        | Inr (k, _) -> OApp (HVar (k-1), List.map (fun m -> clos (s, m)) l)

  and spine = function
    | OLam (x, n), m :: l -> spine (obj (subs_cons ([|m|], subs_id 0)) n, l)
    | n, [] -> n
    | _, _::_ -> assert false

end

let prj = function
  | XLam (x, t) -> OLam (x, t)
  | XApp (h, l) -> OApp (h, l)
  | XMeta (x, s) -> OMeta (x, s)
  | XClos (s, m) -> ESubst.obj s m

let (~~) f = prj $> f $> inj

module Env = struct
  type t = (string option * fam) list
  let empty = []
  let length = List.length
  let find x l = snd (List.nth l x)
  let add x a l = ((x, a) :: l)
  let to_list l = l
  let names_of env = fst (List.split (to_list env))

end

module Sign = struct

  module MO = Map.Make(OConst)
  module MF = Map.Make(FConst)

  type t = (bool * fam) MO.t * kind MF.t
  let empty = MO.empty, MF.empty
  let slices x (o, f) = fst (MO.find x o)
  let ofind x ((o, f):t) = snd (MO.find x o)
  let ffind x ((o, f):t) = MF.find x f
  let oadd x a ((o, f):t) = MO.add x a o, f
  let fadd x k ((o, f):t) = o, MF.add x k f
end

module Strat = struct

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj

  open SLF
  open Util

  let rec app sign env l = function
    | Ident x ->
      begin
        try let i = List.index (Some x) env in
            Obj (inj $ OApp (HVar i, l))
        with Not_found ->
          try let x = OConst.make x in
              ignore (Sign.ofind x sign);
              Obj (inj $ OApp (HConst x, l))
          with Not_found ->
            let x = FConst.make x in
            try ignore(Sign.ffind x sign); Fam (FApp (x, l))
            with Not_found -> failwith ("strat: not found "^FConst.repr x)
      end
    | App (t, u) ->
      begin match term sign env u with
        | Obj u ->  app sign env (u :: l) t
        | _ -> failwith "strat: argument is not an object"
      end
    | _ -> failwith "strat: app error"

  and term sign env = function
    | Lam (x, t) -> Obj (inj $ OLam (x, obj sign (x :: env) t))
    | Type -> Kind KType
    | Prod (x, a, b) ->
      begin match term sign env a, term sign (x::env) b with
        | Fam a, Kind k -> Kind (KProd (x, a, k))
        | Fam a, Fam b -> Fam (FProd (x, a, b))
        | Kind _, _ -> failwith "strat: prod argument is a kind"
        | Fam _, Obj _ -> failwith "strat: prod body is an obj"
        | Obj _, _ -> failwith "strat: prod argument is an obj"
      end
    | App _ as a -> app sign env [] a
    | Ident x ->
      begin try Obj (inj $ OApp (HVar (List.index (Some x) env), []))
        with Not_found ->
          try let x = OConst.make x in
              ignore(Sign.ofind x sign); Obj (inj $ OApp (HConst x, []))
          with Not_found ->
            let x = FConst.make x in
            try ignore(Sign.ffind x sign); Fam (FApp (x, []))
            with Not_found -> failwith ("strat: not found "^FConst.repr x)
      end
    | Meta (x, s) ->
      let s = List.map (obj sign env) s in
      Obj (inj $ OMeta (Meta.make x, s))

  and obj sign env t = match term sign env t with
    | Obj m -> m
    | _ -> failwith "strat: not an obj"

  let fam sign env t = match term sign env t with
    | Fam a -> a
    | _ -> failwith "strat: not a fam"

  let kind sign env t = match term sign env t with
    | Kind k -> k
    | _ -> failwith "strat: not a kind"
end

module Unstrat = struct

  open SLF

  let rec obj env = prj $> function
    | OLam (x, m) -> Lam (x, obj (x :: env) m)
    | OApp (h, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (head env h)) l
    | OMeta (x, s) -> Meta (Names.Meta.repr x, List.map (obj env) s)

  and head env = function
    | HConst c -> Names.OConst.repr c
    | HVar x ->
      try match List.nth env x with
        | Some x -> x
        | None -> "___"^(string_of_int x)
      with Failure "nth" -> "_REL_"^(string_of_int x)

  let rec fam env = function
    | FApp (f, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (Names.FConst.repr f)) l
    | FProd (x, a, b) -> Prod (x, fam env a, fam (x :: env) b)

  let rec kind env = function
    | KType -> Type
    | KProd (x, a, b) -> Prod (x, fam env a, kind (x :: env) b)

end

module Printer = struct

  let eobj e fmt m = SLF.Printer.term fmt (Unstrat.obj e m)
  let efam e fmt a = SLF.Printer.term fmt (Unstrat.fam e a)
  let ekind e fmt k = SLF.Printer.term fmt (Unstrat.kind e k)

  let obj fmt m = eobj [] fmt m
  let fam fmt m = efam [] fmt m
  let kind fmt m = ekind [] fmt m

  let entity fmt = function
    | Strat.Kind k -> kind fmt k
    | Strat.Fam a -> fam fmt a
    | Strat.Obj m -> obj fmt m

  let sign fmt (s : Sign.t) =
    let l =
      Sign.MO.fold
      (fun x (b, a) l -> (Names.OConst.repr x, Unstrat.fam [] a, b) :: l) (fst s)
      (Sign.MF.fold
         (fun x k l -> (Names.FConst.repr x, Unstrat.kind [] k, true) :: l) (snd s)
         []) in
    SLF.Printer.sign fmt l

  let env fmt e =
    let open Format in
    let var fmt = function
      | Some x -> fprintf fmt "%s" x
      | None -> fprintf fmt "_" in
    let rec aux (l:Env.t) fmt = function
      | [] -> ()
      | [x,a] -> fprintf fmt "@[%a@ :@ %a@]" var x (efam (Env.names_of l)) a
      | (x,a) :: e -> fprintf fmt "%a,@ %a" (aux l) [x, a] (aux ((x, a) :: l)) e
    in
    Format.fprintf fmt "@[%a@]" (aux []) (List.rev e)
end

module Lift = struct

  let rec obj k n m =
    let s = subs_liftn (pred k) (subs_shft (n, subs_id 0)) in
    ESubst.clos (s, m)

  let rec fam k n = function
    | FProd (x, a, b) -> FProd (x, fam k n a, fam (k+1) n b)
    | FApp (c, l) -> FApp (c, List.map (obj k n) l)

end

module Subst = struct

  let obj l m =
    let s = subs_cons (Array.of_list (List.rev l), subs_id 0) in
    ESubst.clos (s, m)

  let obj l m =
    let r = obj l m in
    (* Format.printf "** subst [%a] (%a) = @[%a@]@." (Print.pr_list Print.pr_comma Printer.obj) l Printer.obj m Printer.obj r; *)
    r

  let rec fam s = function
    | FApp (c, l) -> FApp (c, List.map (ESubst.obj s $> inj) l)
    | FProd (x, a, b) -> FProd (x, fam s a, fam (subs_lift s) b)

  let rec kind s = function
    | KType -> KType
    | KProd (x, a, k) -> KProd (x, fam s a, kind (subs_lift s) k)

  let fam l m =
    let s = subs_cons (Array.of_list (List.rev l), subs_id 0) in
    let r = fam s m in
    (* Format.printf "** subst [%a] (%a) = @[%a@]@." (Print.pr_list Print.pr_comma Printer.obj) l Printer.fam m Printer.fam r; *)
    r

  let kind l m =
    let s = subs_cons (Array.of_list (List.rev l), subs_id 0) in
    let r = kind s m in
    (* Format.printf "** subst [%a] (%a) = @[%a@]@." (Print.pr_list Print.pr_comma Printer.obj) l Printer.kind m Printer.kind r; *)
    r

end

module Util = struct

  let rec map_meta f = ~~ function
    | OApp (h, l) -> OApp (h, List.map (map_meta f) l)
    | OLam (x, m) -> OLam (x, map_meta f m)
    | OMeta (x, s) -> prj $ f x s

end

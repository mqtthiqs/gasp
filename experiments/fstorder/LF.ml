open Util
open Names
open Esubst

type binder = string option

type head =
  | HVar of int
  | HConst of OConst.t

type obj =
  | XLam of binder * obj
  | XApp of head * spine
  | XMeta of Meta.t * subst
  | XClos of obj subs * obj

and spine = obj list
and subst = obj list

type fam =
  | FApp of FConst.t * spine
  | FProd of binder * fam * fam

type kind =
  | KType
  | KProd of binder * fam * kind

type cobj =
  | OLam of binder * obj
  | OApp of head * spine
  | OMeta of Meta.t * subst

let inj = function
  | OLam (x, t) -> XLam (x, t)
  | OApp (h, l) -> XApp (h, l)
  | OMeta (x, s) -> XMeta (x, s)

exception Not_eta of obj * spine

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
    | XApp (HVar n, l) ->
      let l = List.map (fun m -> clos (s, m)) l in
      match expand_rel (n+1) s with
        | Inl (k, m) -> spine (obj (subs_shft (k, subs_id 0)) m, l)
        | Inr (k, _) -> OApp (HVar (k-1), l)

  and spine = function
    | OLam (x, n), m :: l -> spine (obj (subs_cons ([|m|], subs_id 0)) n, l)
    | (OApp _ | OMeta _) as n, [] -> n
    | n, l -> raise (Not_eta (inj n, l))

end

let prj = function
  | XLam (x, t) -> OLam (x, t)
  | XApp (h, l) -> OApp (h, l)
  | XMeta (x, s) -> OMeta (x, s)
  | XClos (s, m) -> ESubst.obj s m

let mkApp (x, t) = inj @@ OApp (x, t)
let mkLam (x, t) = inj @@ OLam (x, t)
let mkMeta (x, t) = inj @@ OMeta (x, t)

module Lift = struct

  let rec obj k n m =
    assert (k >= 0 && n >= 0);
    let s = subs_liftn (pred k) (subs_shft (n, subs_id 0)) in
    ESubst.clos (s, m)

  let rec fam k n = function
    | FProd (x, a, b) -> FProd (x, fam k n a, fam (k+1) n b)
    | FApp (c, l) -> FApp (c, List.map (obj k n) l)

end

module Subst = struct

  let spine m l =
    inj @@ ESubst.spine (prj @@ m, l)

  let obj l m =
    let s = subs_cons (Array.of_list (List.rev l), subs_id 0) in
    ESubst.clos (s, m)

  let obj l m =
    let r = obj l m in
    (* Debug.log "subst" "[%a] (%a) = @[%a@]@." (Print.list Print.comma SLF.Printer.obj) l SLF.Printer.obj m SLF.Printer.obj r; *)
    r

  let rec fam s = function
    | FApp (c, l) -> FApp (c, List.map (ESubst.obj s @> inj) l)
    | FProd (x, a, b) -> FProd (x, fam s a, fam (subs_lift s) b)

  let rec kind s = function
    | KType -> KType
    | KProd (x, a, k) -> KProd (x, fam s a, kind (subs_lift s) k)

  let fam l m =
    let s = subs_cons (Array.of_list (List.rev l), subs_id 0) in
    let r = fam s m in
    (* Debug.log "subst" "[%a] (%a) = @[%a@]@." (Print.pr_list Print.pr_comma Printer.obj) l Printer.fam m Printer.fam r; *)
    r

  let kind l m =
    let s = subs_cons (Array.of_list (List.rev l), subs_id 0) in
    let r = kind s m in
    (* Debug.log "[%a] (%a) = @[%a@]@." (Print.list Print.comma SLF.Printer.obj) l Printer.kind m Printer.kind r; *)
    r

end

module Lower = struct

  let dummy_obj = mkApp (HConst (OConst.make "YOU_SHOULD_NOT_SEE_THIS"), [])

  let fam n a =
    let s = List.make (fun _ -> dummy_obj) n in
    Subst.fam s a

end

module Util = struct

  let rec map_meta f = prj @> begin function
    | OApp (h, l) -> OApp (h, List.map (map_meta f) l)
    | OLam (x, m) -> OLam (x, map_meta f m)
    | OMeta (x, s) -> prj @@ f x s
  end @> inj

  let fv m =
    let module S = Set.Make(struct type t=int let compare = Pervasives.compare end) in
    let rec fv k s = prj @> function
      | OApp (HVar i, l) when i >= k ->
        List.fold_left (fv k) (S.add (i-k) s) l
      | OApp (_, l) -> List.fold_left (fv k) s l
      | OLam (x, m) -> fv (succ k) s m
      | OMeta (x, l) -> List.fold_left (fv k) s l in
    S.elements (fv 0 S.empty m)


  let eta_expand_var i a =
    let rec exp i l = function
      | FProd (x, a, b) ->
        let l = List.map (Lift.obj 0 1) l in
        let m = exp (succ i) (exp 0 [] a :: l) b in
        mkLam (x, m)
      | FApp _ -> mkApp (HVar i, l)
    in exp i [] a

end

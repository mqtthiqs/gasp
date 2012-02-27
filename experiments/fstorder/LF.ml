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

type entry_type =
  | Sliceable
  | Non_sliceable
  | Defined of (obj list -> obj)

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

  type t = (fam * entry_type) MO.t * kind MF.t
  let empty = MO.empty, MF.empty
  let ofind x ((o, f):t) = MO.find x o
  let ffind x ((o, f):t) = MF.find x f
  let oadd x a ((o, f):t) = MO.add x a o, f
  let fadd x k ((o, f):t) = o, MF.add x k f
  let fold f1 f2 ((o, f):t) (acc : 'a) : 'a = MO.fold f1 o (MF.fold f2 f acc)
end

module Printer = struct
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
    | FApp (c, l) -> FApp (c, List.map (ESubst.obj s @> inj) l)
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

  let rec map_meta f = prj @> begin function
    | OApp (h, l) -> OApp (h, List.map (map_meta f) l)
    | OLam (x, m) -> OLam (x, map_meta f m)
    | OMeta (x, s) -> prj @@ f x s
  end @> inj

end

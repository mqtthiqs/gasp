open Names

type head =
  | HConst of Names.OConst.t
  | HVar of int

type fam =
  | FApp of Names.FConst.t * obj list
  | FArr of fam * fam
  | FProd of fam * fam

and obj =
  | OApp of spine
  | OMeta of Meta.t

and spine = head * obj list

type kind =
  | KType
  | KProd of fam * kind

type env = fam list

type entry =
  | OConst of fam
  | FConst of kind

module Subst = struct

  let rec lift = function
    | OApp (HVar x, l) -> OApp (HVar (x+1), List.map lift l)
    | OApp (h, l) -> OApp (h, List.map lift l)
    | m -> m

  let lift (h, l) = match lift (OApp(h, l)) with OApp(h, l) -> h, l | _ -> assert false

  let rec obj (h,l) = function
    | OApp (HVar 0, l') -> OApp (h, l @ List.map (obj (h,l)) l')
    | m -> m

  let rec fam m = function
    | FApp (c, l) -> FApp (c, List.map (obj m) l)
    | FArr (a, b) -> FProd (fam m a, fam m b)
    | FProd (a, b) -> FProd (fam m a, fam (lift m) b)

  let rec kind m = function
    | KType -> KType
    | KProd (a, k) -> KProd (fam m a, kind (lift m) k)
end

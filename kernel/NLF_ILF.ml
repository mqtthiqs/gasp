open Util
open ILF
open ILF

let map on_fam on_obj defs =
  List.fold_left (fun def (x, ty, t) -> 
    match t with
      | None -> Definitions.declare x (on_fam ty) def
      | Some t -> Definitions.define x (on_obj t) (on_fam ty) def)
    (Definitions.empty ())
    (NLF.Definitions.as_list defs)

let map_construct on_fam on_obj on_term = function
  | NLF.Definitions.Open (x, t) -> 
    Definitions.Open (x, on_term t)
  | NLF.Definitions.Define (defs, t) -> 
    Definitions.Define (map on_fam on_obj defs, on_term t)

let rec fam = function
  | NLF.FConst f -> FConst f
  | NLF.FProd (x, a, b) -> FProd (x, fam a, fam b)
  | NLF.FApp (a, args) -> FApp (fam a, List.map head args)
  | NLF.FDef d -> FDef (map_construct (Option.map fam) obj fam d)
    
and obj = function
  | NLF.OLam (x, a, t) -> OLam (x, fam a, obj t)
  | NLF.OApp (h, args) -> OApp (head h, List.map head args)
  | NLF.ODef d -> ODef (map_construct (Option.map fam) obj obj d)
  | NLF.OConst o -> OConst o
  | NLF.OVar x -> OVar x

and head = function
  | NLF.HVar x -> OVar x
  | NLF.HConst x -> OConst x

and kind = function
  | NLF.KType -> KType
  | NLF.KProd (x, a, k) -> KProd (x, fam a, kind k)

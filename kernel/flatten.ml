open Util
open ILF
open ILF

let not_fapp = function FApp _ -> false | _ -> true

let not_oapp = function OApp _ -> false | _ -> true

let atom x = (x, NLF.Definitions.empty ())

let map on_fam on_obj defs =
  List.fold_left (fun def (x, ty, t) -> 
    match t with
      | None -> NLF.Definitions.declare x (on_fam ty) def
      | Some t -> NLF.Definitions.define x (on_obj t) (on_fam ty) def)
    (NLF.Definitions.empty ())
    (Definitions.as_list defs)

let map_construct on_fam on_obj on_term = function
  | Definitions.Open (x, t) -> 
    NLF.Definitions.Open (x, on_term t)
  | Definitions.Define (defs, t) -> 
    NLF.Definitions.Define (map on_fam on_obj defs, on_term t)
  
let rec fam : ILF.fam -> NLF.fam = function
  | FConst f -> 
    NLF.FConst f
  | FProd (x, a, b) -> 
    NLF.FProd (x, fam a, fam b)
  | FApp (a, args) -> 
    assert (not_fapp a);
    let args_defs, args = name_arguments args in
    NLF.FDef (NLF.Definitions.Define (args_defs, NLF.FApp (fam a, args)))
  | FDef d -> 
    NLF.FDef (map_construct (Option.map fam) obj fam d)

and obj = function
  | OConst o -> 
    NLF.OConst o
  | OVar x -> 
    NLF.OVar x
  | OLam (x, a, t) -> 
    NLF.OLam (x, fam a, obj t)
  | OApp (a, args) -> 
    assert (not_oapp a);
    let defs, args = name_arguments args in
    let defs, a = name_obj defs a in
    NLF.ODef (NLF.Definitions.Define (defs, NLF.OApp (a, args)))
  | ODef d -> 
    NLF.ODef (map_construct (Option.map fam) obj obj d)

and fresh_def defs o =
  let x = Name.gen_variable () in
  NLF.Definitions.define x o None defs, NLF.HVar x

and name_obj defs = function
  | OConst o -> defs, NLF.HConst o
  | OVar x -> defs, NLF.HVar x
  | o -> fresh_def defs (obj o)

and name_arguments args = 
  let name_argument (defs, args) o =
    let defs, h = name_obj defs o in
    (defs, h :: args)
  in
  let defs, args = 
    List.fold_left name_argument (NLF.Definitions.empty (), []) args
  in
  defs, List.rev args

and kind = function
  | KType -> NLF.KType
  | KProd (x, a, k) -> NLF.KProd (x, fam a, kind k)

let entity = function
  | Kind k -> NLF.Kind (kind k)
  | Fam f -> NLF.Fam (fam f)
  | Obj o -> NLF.Obj (obj o)



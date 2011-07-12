open Util
open ILF
open ILF

let atom x = (x, [])

let rec fam = function
  | FConst f -> 
    atom (FConst f)
  | FProd (x, a, b) -> 
    atom (FProd (x, close_fam a, close_fam b))
  | FApp (a, args) -> 
    let a, args' = fam a in 
    (a, args' @ (List.map close_obj args))
  | FDef d -> 
    atom (FDef (Definitions.map_construct (Option.map close_fam) close_obj close_fam d))

and close_fam a = 
  let a, args = fam a in FApp (a, args)

and obj = function
  | OConst o -> 
    atom (OConst o)
  | OVar x -> 
    atom (OVar x)
  | OLam (x, a, t) -> 
    atom (OLam (x, close_fam a, close_obj t))
  | OApp (a, args) -> 
    let a, args' = obj a in 
    (a, args' @ (List.map close_obj args))
  | ODef d -> 
    atom (ODef (Definitions.map_construct (Option.map close_fam) close_obj close_obj d))

and close_obj a = 
  let a, args = obj a in
  OApp (a, args)

and kind = function
  | KType -> KType
  | KProd (x, a, k) -> KProd (x, close_fam a, kind k)

let fam = close_fam

let obj = close_obj

let entity = function
  | Kind k -> Kind (kind k)
  | Fam f -> Fam (close_fam f)
  | Obj o -> Obj (close_obj o)


open Util
open ILF
open ILF
let ( @@ ) = NLF.Definitions.( @@ )

let not_fapp = function FApp _ -> false | _ -> true

let not_oapp = function OApp _ -> false | _ -> true

let atom x = (NLF.Definitions.empty (), x)

let map on_fam on_obj defs =
  List.fold_left (fun def (x, ty, t) -> 
    match t with
      | None -> 
	let defs, ty = on_fam ty in
	NLF.Definitions.declare x ty (def @@ defs)
      | Some t -> 
	let t_defs, t = on_obj t in
	let a_defs, a = on_fam ty in
	NLF.Definitions.define x t a (def @@ t_defs @@ a_defs))
    (NLF.Definitions.empty ())
    (Definitions.as_list defs)

let map_construct mk on_fam on_obj on_term refresh_term = function
  | Definitions.Open (x, t) -> 
    (* FIXME: Wrong. *)
    let defs, t = on_term t in
    (defs, mk (NLF.Definitions.Open (x, t)))

  | Definitions.Define (ndefs, t) -> 
    let ndefs, t = 
      Refresh.alpha_rename_define ndefs refresh_term t 
    in
    let ndefs = map on_fam on_obj ndefs in
    let defs, t = on_term t in
    (ndefs @@ defs, t)

let define mk defs t = 
  match NLF.Definitions.as_list defs with
    | [] -> t
    | _ -> mk (NLF.Definitions.Define (defs, t))
  
let rec fam = function
  | FConst f -> 
    atom (NLF.FConst f)
  | FProd (x, a, b) -> 
    let (x, a, b)  = Refresh.alpha_rename_prod x a b in
    let defs, a = fam a in
    defs, NLF.FProd (x, a, close_fam b)
  | FApp (FConst x, args) -> 
    let args_defs, args = name_arguments args in
    args_defs, NLF.FApp (x, args)
  | FDef d -> 
    map_construct (fun x -> NLF.FDef x) opt_fam obj fam Refresh.fam d
  | t -> 
    (* There is not structured term in functional position. *)
    Format.fprintf Format.std_formatter "@[%a@]" Pp.pp_fam t;
    assert false

and opt_fam = function
  | None -> NLF.Definitions.empty (), None
  | Some a -> let defs, a = fam a in defs, Some a

and obj : obj -> NLF.definitions * NLF.obj = function
  | OConst o -> 
    atom (NLF.OConst o)
  | OVar x -> 
    atom (NLF.OVar x)
  | OLam (x, a, t) -> 
    let (x, a, t)  = Refresh.alpha_rename_lam x a t in
    atom (NLF.OLam (x, close_fam a, close_obj t))
  | OApp (a, args) -> 
    assert (not_oapp a);
    let defs, args = name_arguments args in
    let odefs, a = name_obj a in
(*    Format.eprintf "@\n@[%a AND %a@]@\n@." 
      NLF.Pp.pp_definitions defs NLF.Pp.pp_definitions odefs; *)
    (defs @@ odefs, NLF.OApp (a, args))
  | ODef d -> 
    map_construct (fun x -> NLF.ODef x) opt_fam obj obj Refresh.obj d

and close_obj o = 
  let defs, r = obj o in 
  let r = define (fun x -> NLF.ODef x) defs r in
(*  Format.eprintf "@\n@[Close @[%a@]@ @;-> @[%a@]@]@\n@." 
    Pp.pp_obj o
    NLF.Pp.pp_obj r; *)
  r

and close_fam f = 
(*   Format.printf "@[CLOSE %a@]@." Pp.pp_fam f; *)
  let defs, f = fam f in 
  define (fun x -> NLF.FDef x) defs f

and fresh_def defs o =
  let x = Name.gen_variable () in
  NLF.Definitions.define x o None defs, NLF.HVar x

and name_obj : ILF.obj -> NLF.definitions * NLF.head = function
  | OConst o -> NLF.Definitions.empty (), NLF.HConst o
  | OVar x -> NLF.Definitions.empty (), NLF.HVar x
  | o -> 
    let odefs, o = obj o in 
    fresh_def odefs o

and name_arguments args = 
  let name_argument (defs, args) o =
    let odefs, h = name_obj o in
    (defs @@ odefs, h :: args)
  in
  let defs, args = 
    List.fold_left name_argument (NLF.Definitions.empty (), []) args
  in
  defs, List.rev args

and kind = function
  | KType -> NLF.KType
  | KProd (x, a, k) -> NLF.KProd (x, close_fam a, kind k)

let entity = function
  | Kind k -> NLF.Kind (kind k)
  | Fam f -> NLF.Fam (close_fam f)
  | Obj o -> NLF.Obj (close_obj o)

let obj = close_obj

let fam = close_fam

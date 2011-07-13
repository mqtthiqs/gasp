open Util
open LF

module Make (LF: Sig) =
struct
  open LF

  module Renaming = Name.Varmap

  type renaming = Name.variable Renaming.t

  let rename r x = 
    try Renaming.find x r with Not_found -> x

  module Refresh (Ext : sig
    val fhead : renaming -> fhead -> fhead
    val head  : renaming -> head -> head
  end) = struct

    open Ext

    let binder' on_fam r x a = 
      let a = on_fam a in
      let r = Renaming.remove x r in
      (a, r)

    let rec definitions r on_fam on_obj defs = 
      let definition (ndefs, r) (x, a, t) =
	let (a, r) = binder' (Option.map (on_fam r)) r x a in
	let ndefs = 
	  match t with
	    | None -> Definitions.declare x a ndefs
	    | Some t -> Definitions.define x (on_obj r t) a ndefs
	in
	(ndefs, r)
      in
      List.fold_left definition (Definitions.empty (), r) (Definitions.as_list defs)

    let rec definitions_construct r on_fam on_obj on_term = function
      | Definitions.Open (x, t) ->
	Definitions.Open (rename r x, on_term r t)
      | Definitions.Define (defs, t) ->
	let (defs, r) = definitions r on_fam on_obj defs in 
	Definitions.Define (defs, on_term r t)

    let rec fam r = function
      | FProd (x, a, b) -> 
	let (a, r) = binder r x a in
	FProd (x, a, fam r b)
      | FApp (h, args) -> 
	FApp (fhead r h, arguments r args)
      | FDef d -> 
	FDef (definitions_construct r fam obj fam d)
      | x -> 
	x

    and obj r = function
      | OVar x -> 
	OVar (rename r x)
      | OLam (x, a, o) -> 
	let (a, r) = binder r x a in
	OLam (x, a, obj r o)
      | OApp (h, args) -> 
	OApp (head r h, arguments r args)
      | ODef d ->
	ODef (definitions_construct r fam obj obj d)
      | x -> 
	x

    and kind r = function
      | KType -> 
	KType
      | KProd (x, a, k) -> 
	let (a, r) = binder r x a in
	KProd (x, a, kind r k)
	
    and arguments r args = 
      List.map (head r) args

    and binder r = binder' (fam r) r

    let fresh_binder ?(into=Name.gen_variable ()) r x = 
      let r = Renaming.add x into r in
      (r, into)

    let alpha_rename_lam x ?into ty t = 
      let (r, y) = fresh_binder Renaming.empty ?into x in
      (y, fam r ty, obj r t)

    let alpha_rename_prod x ?into ty a = 
      let (r, y) = fresh_binder Renaming.empty ?into x in
      (y, fam r ty, fam r a)

    let alpha_rename_kind_prod x ?into ty k = 
      let (r, y) = fresh_binder Renaming.empty ?into x in
      (y, fam r ty, kind r k)

    let alpha_rename_define defs on_term t = 
      let ldefs = Definitions.as_list defs in
      let xs = List.map (fun (x, _, _) -> x) ldefs in
      let r, ys = list_fold_map fresh_binder Renaming.empty xs in
      let definition ndefs (_, a, t) y =
	let a = Option.map (fam r) a in
	match t with
	  | None -> Definitions.declare y a ndefs
	  | Some t -> Definitions.define y (obj r t) a ndefs
      in
      (List.fold_left2 definition (Definitions.empty ()) ldefs ys, on_term r t)

  end

end

open Name
open NLF

module S = NLFSubst

let subst_of = function NLF.Obj(s,_) -> s
let with_subst s = function NLF.Obj(_,r) -> NLF.Obj(s,r)

let rec obj term = function
  | XLFe.OLam (x, a, t) ->
    let a = fam term a in
    NLF.Obj(S.empty, NLF.VLam(x, a, obj term t))
  | XLFe.OMeta (x, XLFe.FConst(c,m)) ->
    let sigma, m = args term m in
    let y = gen_variable() in
    let sigma = S.add y (XLF.HVar x, [], c, m) sigma in
    NLF.Obj (sigma, NLF.VHead(XLF.HVar y, c, m))
  | XLFe.OHead (h,l,XLFe.FConst(c,m)) ->
    let sigma, l = args term l in
    let sigma, m = args (with_subst sigma term) m in
    let y = gen_variable() in
    let sigma = S.add y (h, l, c, m) sigma in
    NLF.Obj(sigma, NLF.VHead(XLF.HVar y, c, m))
  | XLFe.OBox(t,p,u) ->
    let u = obj term u in
    let term = go term p (ignore u; assert false) in          (* TODO *)
    obj term t

and arg term (x,t) : S.t * (variable * NLF.value) = match t with
  | XLFe.OHead(h,l,XLFe.FConst(c,m)) ->
    let sigma, m = args term m in
    if l = [] then
      S.empty, (x, NLF.VHead (h, c, m))		(* empty? et le type?? TODO *)
    else
      let z = Name.gen_variable() in
      let sigma, l = args (with_subst sigma term) l in
      S.add z (h, l, c, m) sigma,
      (x, NLF.VHead (XLF.HVar z, c, m))
  | XLFe.OLam (x,a,t) ->
    let t = obj term t in
    let a = fam term a in
    subst_of term, (x, NLF.VLam(x,a,t))
  | XLFe.OMeta(z,XLFe.FConst(c,m)) -> 
      let sigma, m = args term m in
      S.empty, (x, NLF.VHead(XLF.HVar z, c, m)) (* empty? et le type?? TODO *)
  | XLFe.OBox(t,p,u) ->
    let u = obj term u in
    let term = go term p (ignore u; assert false) in (* TODO *)
    arg term (x,t)    (* TODO subst *)

and args term : XLFe.args -> S.t * NLF.args =  function
  | [] -> subst_of term, []
  | e :: l -> 
      let sigma, (x,t) = arg term e in
      let sigma, l = args (with_subst sigma term) l in
      sigma, (x, t) :: l

and fam term = function
  | XLFe.FProd (x,a,b) ->
    NLF.FProd(x, fam term a, fam term b)
  | XLFe.FHead(XLFe.FConst(c,l)) ->
      let sigma, fargs = args term l in
      NLF.FHead(sigma, c, fargs)

let rec kind term = function
  | XLFe.KProd(x, a, k) ->
    NLF.KProd(x, fam term a, kind term k)
  | XLFe.KType -> NLF.KType

let entry kont nlfs = function 
    | XLFe.ODecl a -> kont nlfs (NLF.ODecl (fam bidon a))
    | XLFe.FDecl k -> kont nlfs (NLF.FDecl (kind bidon k))

(* and back *)

let s_merge s1 s2 =
  S.fold S.add s1 s2

let rec from_obj sigma = function
  | NLF.Obj(sigma', v) ->
    let sigma = s_merge sigma sigma' in
    from_val sigma v

    (* E.fold (fun x a t -> XLFe.OLam(x, from_fam sigma a, t)) env *)
    (*   (XLFe.OHead(h, from_args sigma args, XLFe.FConst(c, from_args sigma fargs))) *)

  (* | NLF.OMeta(env, sigma', x, c, fargs) as t -> *)
  (*   let sigma = s_merge sigma sigma' in *)
  (*   let h, oa, c, fa = *)
  (*     try S.find x sigma *)
  (*     with Not_found -> *)
  (* 	Format.printf "%a not found in %a@." Name.Pp.variable x Pp.obj (with_subst sigma t); *)
  (* 	Error.global_error "definition not found" "" in *)
  (*   E.fold (fun x a t -> XLFe.OLam(x, from_fam sigma a, t)) env *)
  (*     (XLFe.OHead(h, from_args sigma oa, XLFe.FConst(c, from_args sigma fa))) *)

and from_val sigma = function
  | NLF.VLam(x, a, t) ->
    XLFe.OLam (x, from_fam sigma a, from_obj sigma t)
  | NLF.VHead(h, c, m) ->
    let m = from_args sigma m in
    XLFe.OHead(h, [], XLFe.FConst(c, m))
  (* | NLF.VHead(XLF.HVar x, _, _) -> *)
  (*   try *)
  (*     let (h, l, c, m) = S.find x sigma in *)
  (*     XLFe.OHead(h, from_args sigma l, XLFe.FConst(c, from_args sigma m)) *)
  (*   with Not_found -> *)
  (*     raise (failwith ("from_val: Not_found "^(Name.of_variable x))) *)

and from_args sigma args =
  List.map (fun x, v -> x, from_val sigma v) args

and from_fam sigma = function
  | NLF.FProd(x, a, b) -> XLFe.FProd(x, from_fam sigma a, from_fam sigma b)
  | NLF.FHead(sigma', c, l) ->
    let sigma = s_merge sigma sigma' in
    let l = from_args sigma l in
    XLFe.FHead(XLFe.FConst(c,l))

let rec from_kind = function
  | NLF.KProd(x, a, b) -> XLFe.KProd(x, from_fam S.empty a, from_kind b)
  | NLF.KType -> XLFe.KType

let from_obj = from_obj S.empty
let from_fam = from_fam S.empty

let rec from_sign s = NLFSign.fold
  (fun x e s -> (x, match e with
    | NLF.FDecl k -> XLFe.FDecl (from_kind k)
    | NLF.ODecl a -> XLFe.ODecl (from_fam a)) :: s
  ) s []

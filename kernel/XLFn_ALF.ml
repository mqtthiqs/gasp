open Name
open ALF

module S = ALFSubst

let with_subst sigma : ALF.obj -> ALF.obj = function
  | ALF.OLam _ -> assert false
  | ALF.OHead(_, h, l, c, m) -> ALF.OHead (sigma, h, l, c, m)
  | ALF.OMeta(_, x, c, m) -> ALF.OMeta (sigma, x, c, m)

let subst_of = function
  | ALF.OLam _ -> assert false
  | ALF.OHead(s, h, l, c, m) -> s
  | ALF.OMeta(s, x, c, m) -> s

let rec obj term = function
  | XLFn.OLam(x, a, t) -> ALF.OLam(x, fam term a, obj term t)
  | XLFn.OMeta(x, XLFn.FConst(a, m)) ->
    let sigma, m = args term m in
    ALF.OMeta(sigma, x, a, m)
  | XLFn.OBox _ -> assert false 	(* TODO *)
  | XLFn.OHead(h, l, XLFn.FConst(a, m)) ->
    let sigma, oargs = args term l in
    let sigma, fargs = args (with_subst sigma term) m in
    ALF.OHead(sigma, h, oargs, a, fargs)

and args term : XLFn.args -> ALF.subst * ALF.args = function
  | [] -> subst_of term, []
  | e :: l ->
    let sigma, (x, t) = arg term e in
    let sigma, l = args (with_subst sigma term) l in
    sigma, (x, t) :: l

and arg term (x,t) : ALF.subst * (variable * ALF.arg) = match t with
  | XLFn.OHead (h, l, XLFn.FConst (c, m)) ->
      let sigma, fargs = args term m in
      if l = [] then
        sigma, (x, ALF.AHead(h, c, fargs))
      else 
        let z = Name.gen_definition() in
	let sigma, oargs = args (with_subst sigma term) l in
	S.add z (h, oargs, c, fargs) sigma,
	(x, ALF.AMeta(z, c, fargs))
  | XLFn.OLam (y,a,t) ->
      subst_of term, (x, ALF.ALam(y, fam term a, obj term t))
  | XLFn.OMeta(z,XLFn.FConst(c,m)) -> 
      let sigma, fargs = args term m in
      sigma, (x, ALF.AMeta(z, c, fargs))
  | XLFn.OBox(t,p,s) -> assert false

and fam term = function
  | XLFn.FProd(x,a,b) -> ALF.FProd(x, fam term a, fam term b)
  | XLFn.FHead(XLFn.FConst(c,l)) ->
    let sigma, l = args term l in
    ALF.FHead(ALF.FConst(sigma, c, l))

(* and back *)

let s_merge s1 s2 =
  S.fold S.add s1 s2

let rec from_obj sigma = function
  | ALF.OLam(x,a,t) ->
    XLFn.OLam(x, from_fam sigma a, from_obj sigma t)
  | ALF.OHead(sigma', h, args, c, fargs) ->
    let sigma = s_merge sigma sigma' in
    XLFn.OHead(h, from_args sigma args, XLFn.FConst(c, from_args sigma fargs))
  | ALF.OMeta(sigma', x, c, fargs) as t ->
    let sigma = s_merge sigma sigma' in
    let h, oa, c, fa =
      try S.find x sigma
      with Not_found ->
	Format.printf "%a not found in %a@." Name.Pp.definition x Pp.obj (with_subst sigma t);
	Error.global_error "definition not found" "" in
    XLFn.OHead(h, from_args sigma oa, XLFn.FConst(c, from_args sigma fa))

and from_args sigma args =
  List.map (fun (x, t) -> x, from_arg sigma t) args

and from_arg sigma = function
  | ALF.ALam(x,a,t) -> XLFn.OLam(x, from_fam sigma a, from_obj sigma t)
  | ALF.AHead(h,c,l) -> XLFn.OHead(h, [], XLFn.FConst(c, from_args sigma l))
  | ALF.AMeta(x,c,l) -> XLFn.OMeta(x, XLFn.FConst(c, from_args sigma l))

and from_fam sigma = function
  | ALF.FProd(x,a,b) -> XLFn.FProd(x, from_fam sigma a, from_fam sigma b)
  | ALF.FHead(ALF.FConst(sigma',c,fargs)) ->
    let sigma = s_merge sigma sigma' in
    let l = from_args sigma fargs in
    XLFn.FHead(XLFn.FConst(c,l))

let rec from_kind = function
  | ALF.KType -> XLFn.KType
  | ALF.KProd(x,a,k) -> XLFn.KProd(x, from_fam S.empty a, from_kind k)

let from_obj = from_obj S.empty
let from_fam = from_fam S.empty

let rec from_sign s = ALFSign.fold
  (fun x e s -> (x, match e with
    | ALF.FDecl k -> XLFn.FDecl (from_kind k)
    | ALF.ODecl a -> XLFn.ODecl (from_fam a)) :: s
  ) s []

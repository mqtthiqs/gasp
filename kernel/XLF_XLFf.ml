
let rec name_obj sigma h l =
  let sigma, l = args sigma l in
  let x = Name.gen_variable () in
  (x, (h,l)) :: sigma, XLFf.VHead (XLF.HVar x)

and obj : XLF.obj -> XLFf.obj = function
  | XLF.OLam (x, t) -> XLFf.Obj ([], XLFf.VLam(x, obj t))
  | XLF.OBox (t, p, u) -> XLFf.OBox (obj t, p, obj u)
  | XLF.OHead (h,l) ->
    let sigma, v = name_obj [] h l in
    XLFf.Obj (List.rev sigma, v)

and arg sigma : XLF.obj -> XLFf.subst * XLFf.value = function
  | XLF.OLam (x, t) -> sigma, XLFf.VLam(x, obj t)
  | XLF.OHead (h, []) -> sigma, XLFf.VHead h
  | XLF.OHead (h, l) -> name_obj sigma h l
  | XLF.OBox _ -> failwith "box en arg"

and args sigma : XLF.args -> XLFf.subst * XLFf.args =
  Util.list_fold_map arg sigma

and fam = function
  | XLF.FProd (x, a, b) ->
    XLFf.FProd (x, fam a, fam b)
  | XLF.FConst(c,l) ->
    let sigma, l = args [] l in
    XLFf.FHead (sigma, c, l)

let rec kind = function
  | XLF.KType -> XLFf.KType
  | XLF.KProd (x, a, k) -> XLFf.KProd (x, fam a, kind k)

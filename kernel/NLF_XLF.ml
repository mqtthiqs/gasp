open Name

let rec obj env sigma : NLF.obj -> XLF.obj = function
  | NLF.Obj (sigma', v) ->
    value env (Varmap.fold Varmap.add sigma' sigma) v

and value env sigma : NLF.value -> XLF.obj = function
  | NLF.VHead (Var x, _) ->
    begin
      try def env sigma (Varmap.find x sigma)
      with Not_found ->
	if Varmap.mem x env then XLF.OAtom (Var x, [])
	else failwith ("checkout: not_found "^of_variable x)
    end
  | NLF.VHead (Cst c, _) -> XLF.OAtom (Cst c, [])
  | NLF.VLam (Some x, _, t) ->
    XLF.OLam (Some x, obj (Varmap.add x () env) sigma t)
  | NLF.VLam (None, _, t) ->
    XLF.OLam (None, obj env sigma t)

and def env sigma : NLF.def -> XLF.obj = function
  | NLF.DAtom (h, l, _) -> XLF.OAtom (h, args env sigma l)
  | NLF.DHead (h, _) -> XLF.OAtom(h, [])

and args env sigma l =
  List.fold_left begin
    fun l v -> value env sigma v :: l
  end [] l

let rec fam env = function
  | NLF.FAtom (s, c, m) -> XLF.FAtom(c, args env s m)
  | NLF.FProd (Some x, a, b) -> XLF.FProd(Some x, fam env a, fam (Varmap.add x () env) b)
  | NLF.FProd (None, a, b) -> XLF.FProd(None, fam env a, fam env b)

let rec kind env = function
  | NLF.KType -> XLF.KType
  | NLF.KProd (Some x, a, k) -> XLF.KProd(Some x, fam env a, kind (Varmap.add x () env) k)
  | NLF.KProd (None, a, k) -> XLF.KProd(None, fam env a, kind env k)


let obj t = obj Varmap.empty Varmap.empty t
let fam a = fam Varmap.empty a
let kind k = kind Varmap.empty k

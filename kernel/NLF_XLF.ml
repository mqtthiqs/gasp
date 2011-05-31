open Name

let rec obj env sigma : NLF.obj -> XLF.obj = function
  | NLF.Obj (sigma', v) ->
    value env (Varmap.fold Varmap.add sigma' sigma) v

and value env sigma : NLF.value -> XLF.obj = function
  | NLF.VHead (XLF.HVar x, _) ->
    begin
      try def env sigma (Varmap.find x sigma)
      with Not_found ->
	if Varmap.mem x env then XLF.OAtom (XLF.HVar x, [])
	else failwith ("checkout: not_found "^of_variable x)
    end
  | NLF.VHead (XLF.HConst c, _) -> XLF.OAtom (XLF.HConst c, [])
  | NLF.VLam (x, _, t) ->
    XLF.OLam (x, obj (Varmap.add x () env) sigma t)

and def env sigma : NLF.def -> XLF.obj = function
  | NLF.DAtom (h, l, _) -> XLF.OAtom (h, args env sigma l)
  | NLF.DHead (h, _) -> XLF.OAtom(h, [])

and args env sigma l =
  List.fold_left begin
    fun l v -> value env sigma v :: l
  end [] l

let rec fam env sigma = function
  | NLF.FAtom (v, c, m) -> (* XLF.FAtom(c, args env (Varmap.fold Varmap.add s sigma) m) *)assert false
  | NLF.FProd (x, a, b) -> XLF.FProd(x, fam env sigma a, fam env sigma b)

let rec kind env sigma k = assert false

let obj t = obj Varmap.empty Varmap.empty t
let fam a = fam Varmap.empty Varmap.empty a
let kind k = kind Varmap.empty Varmap.empty k

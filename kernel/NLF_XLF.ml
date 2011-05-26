open Name

let rec obj env sigma : NLF.obj -> XLF.obj = function
  | NLF.Obj (sigma', v) ->
    value env (Varmap.fold Varmap.add sigma' sigma) v

and value env sigma : NLF.value -> XLF.obj = function
  | NLF.VHead (XLF.HVar x,( _, _)) ->
    begin
      try def env sigma (Varmap.find x sigma)
      with Not_found ->
	if Varmap.mem x env then XLF.OHead (XLF.HVar x, [])
	else failwith ("checkout: not_found "^of_variable x)
    end
  | NLF.VHead (XLF.HConst c, (_, _)) -> XLF.OHead (XLF.HConst c, [])
  | NLF.VLam (x, _, t) ->
    XLF.OLam (x, obj (Varmap.add x () env) sigma t)

and def env sigma : NLF.def -> XLF.obj = function
  | NLF.DApp (h, l, (_, _)) -> XLF.OHead (h, args env sigma l)
  | NLF.DHead (h, _) -> XLF.OHead(h, [])

and args env sigma l =
  List.fold_left begin
    fun l (_,v) -> value env sigma v :: l
  end [] l

let obj t = obj Varmap.empty Varmap.empty t

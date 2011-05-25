open Name

let rec obj env : NLF.obj -> XLF.obj = function
  | NLF.Obj (sigma, v) ->
    value (Varmap.fold Varmap.add sigma env) v

and value env : NLF.value -> XLF.obj = function
  | NLF.VHead (XLF.HVar x, _, _) ->
    begin
      try def env (Varmap.find x env)
      with Not_found -> XLF.OHead (XLF.HVar x, [])
    end
  | NLF.VHead (XLF.HConst c, _, _) -> XLF.OHead (XLF.HConst c, [])
  | NLF.VLam (x, _, t) ->
    XLF.OLam (x, obj env t)

and def env : NLF.def -> XLF.obj = function
  | NLF.DApp (h, l, _, _) -> XLF.OHead (h, args env l)
  | NLF.DHead (h, _) -> XLF.OHead(h, [])

and args env l =
  List.fold_left begin
    fun l (_,v) -> value env v :: l
  end [] l

let obj t = obj Varmap.empty t

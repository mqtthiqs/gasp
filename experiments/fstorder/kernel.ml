
module Check = struct

  open LF

  let head sign ctx env : head -> fam = function
    | HConst c -> Repo.Sign.ofind c sign
    | HVar x -> Repo.Env.find x env

  let rec obj sign ctx env = function
    | OApp (h, l) -> app sign ctx env (l, head sign ctx env h)
    | OMeta x -> snd (Repo.Context.find x ctx)

  and app sign ctx env = function
    | [], (FApp _ as a) -> a
    | [], _ -> failwith "not eta-expanded"
    | m :: l, FArr (a, b) ->
      if obj sign ctx env m = a           (* TODO eq *)
      then app sign ctx env (l, b)
      else failwith "type mismatch"
    | OApp s :: l, FProd (a, b) ->
      if obj sign ctx env (OApp s) = a           (* TODO eq *)
      then app sign ctx env (l, Subst.fam s b)
      else failwith "type mismatch"
    | OMeta _ :: _, FProd _ -> failwith "can't use a meta as a dependent argument"
    | _ :: _, FApp _ -> failwith "non-functional application"

  and fapp sign ctx env = function
    | [], KType -> ()
    | _ :: _, KType -> failwith "non-functional application"
    | [], _ -> failwith "not eta-expanded"
    | OApp s :: l, KProd (a, k) ->
      if obj sign ctx env (OApp s) = a           (* TODO eq *)
      then fapp sign ctx env (l, Subst.kind s k)
      else failwith "type mismatch"
    | OMeta _ :: _, KProd _ -> failwith "can't use a meta as a dependent argument"

  let rec fam sign ctx env = function
    | FApp (c, l) -> fapp sign ctx env (l, Repo.Sign.ffind c sign)
    | FArr (a, b) -> fam sign ctx env a; fam sign ctx env b
    | FProd (a, b) -> fam sign ctx env a; fam sign ctx (Repo.Env.add a env) b

  let rec kind sign ctx env = function
    | KType -> ()
    | KProd (a, k) -> fam sign ctx env a; kind sign ctx (Repo.Env.add a env) k

end

let gensym =
  let n = ref 0 in
  fun () -> incr n; string_of_int !n

let push repo m =
  let a = Check.obj repo.Repo.sign repo.Repo.ctx Repo.Env.empty m in
  let x = Names.Meta.make ("X"^gensym()) in
  { repo with Repo.ctx = Repo.Context.add x (m, a) repo.Repo.ctx }


module Check = struct

  open LF
  open Repo

  let rec obj repo env = function
    | OApp (c, l) -> app repo env (l, Sign.ofind c repo.sign)
    | OVar x -> Env.find x env
    | OMeta x -> snd (Repo.Context.find x repo.ctx)

  and app repo env = function
    | [], (FApp _ as a) -> a
    | [], _ -> failwith "not eta-expanded"
    | m :: l, FProd (_, a, b) ->
      if obj repo env m = a           (* TODO eq *)
      then app repo env (l, Subst.fam m b)
      else failwith "type mismatch"
    | _ :: _, FApp _ -> failwith "non-functional application"

  and fapp repo env = function
    | [], KType -> ()
    | _ :: _, KType -> failwith "non-functional application"
    | [], _ -> failwith "not eta-expanded"
    | m :: l, KProd (_, a, k) ->
      if obj repo env m = a           (* TODO eq *)
      then fapp repo env (l, Subst.kind m k)
      else failwith "type mismatch"

  let rec fam repo env = function
    | FApp (c, l) -> fapp repo env (l, Sign.ffind c repo.sign)
    | FProd (_, a, b) -> fam repo env a; fam repo (Env.add a env) b

  let rec kind repo env = function
    | KType -> ()
    | KProd (_, a, k) -> fam repo env a; kind repo (Env.add a env) k

end

let rec init s = function
  | [] -> s
  | (c, t, b) :: s' ->
    let repo = { Repo.sign = s;
                 Repo.ctx = Repo.Context.empty;
                 Repo.head = Names.Meta.make "DUMMY" } in
    match LF.Strat.term s [] t with
      | LF.Strat.Obj _ -> failwith "object in sign"
      | LF.Strat.Fam a ->
        ignore (Check.fam repo LF.Env.empty a);
        init (LF.Sign.oadd (Names.OConst.make c) (b, a) s) s'
      | LF.Strat.Kind k ->
        ignore (Check.kind repo LF.Env.empty k);
        init (LF.Sign.fadd (Names.FConst.make c) k s) s'

let push repo m =
  let gensym =
    let n = ref 0 in
    fun () -> incr n; string_of_int !n in
  let a = Check.obj repo LF.Env.empty m in
  let x = Names.Meta.make ("X"^gensym()) in
  { repo with Repo.ctx = Repo.Context.add x (m, a) repo.Repo.ctx }, x

let pull repo x =
  let rec aux ctx x =
    LF.Util.fold_meta (aux ctx) (fst (Repo.Context.find x ctx))
  in aux repo.Repo.ctx x

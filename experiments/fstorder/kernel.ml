
open LF
open Repo

let pull repo x =
  let rec aux ctx x =
    LF.Util.fold_meta (aux ctx) (fst (Repo.Context.find x ctx))
  in aux repo.Repo.ctx x

let push =
  let gensym =
    let n = ref 0 in
    fun () -> incr n; string_of_int !n in
  fun repo env a (h, l) ->
    let x = Names.Meta.make ("X"^gensym()) in
    let repo = { repo with
      Repo.ctx = Repo.Context.add x (LF.OApp (h, l), a) repo.Repo.ctx;
      Repo.head = x } in
    repo

module Conv = struct

  exception Not_conv of obj * obj

  let head = function
    | HVar i1, HVar i2 when i1 = i2 -> ()
    | HConst c1, HConst c2 when Names.OConst.compare c1 c2 = 0 -> ()
    | h1, h2 -> raise (Not_conv (OApp (h1, []), OApp (h2, [])))

  let rec spine = function
    | [], [] -> ()
    | m1 :: l1, m2 :: l2 -> obj (m1, m2); spine (l1, l2)
    | l1, l2 -> let h = HConst (Names.OConst.make "@") in raise (Not_conv (OApp (h, l1), OApp (h, l2)))

  and obj = function
    | OLam (_, m1), OLam (_,m2) -> obj (m1, m2)
    | OApp (h1, l1), OApp (h2, l2) -> head (h1, h2); spine (l1, l2)
    | OMeta x1, OMeta x2 when Names.Meta.compare x1 x2 = 0 -> ()
    | OMeta _, _ | _, OMeta _ -> failwith "not implemented"
    | m1, m2 -> raise (Not_conv (m1, m2))

  let rec fam = function
    | FProd (_, a1, b1), FProd (_, a2, b2) ->
      fam (a1, a2); fam (b1, b2)
    | FApp (c1, l1), FApp (c2, l2) ->
      if Names.FConst.compare c1 c2 <> 0 then failwith "not convertible";
      spine (l1, l2)
    | _ -> failwith "not convertible"
end

module Check = struct

  let head repo env : head -> fam * bool = function
    | HVar x -> Env.find x env, false
    | HConst c -> Sign.ofind c repo.sign, Sign.slices c repo.sign

  let rec obj repo env : obj * fam -> Repo.t * obj = function
    | OLam (x, m), FProd (_, a, b) ->
      let repo, m = obj repo (Env.add a env) (m, b) in
      repo, OLam (x, m)
    | OLam _, FApp _ -> failwith "not eta"
    | OApp (h, l), a ->
      let b, slices = head repo env h in
      let repo, l, a' = app repo env (l, b) in
      Conv.fam (a, a');
      if slices then
        let repo = push repo env a (h, l) in
        repo, OMeta (repo.head)
      else
        repo, OApp (h, l)
    | OMeta x as m, a ->
      Conv.fam (snd (Repo.Context.find x repo.ctx), a);
      repo, m

  and app repo env : spine * fam -> Repo.t * spine * fam = function
    | [], (FApp _ as a) -> repo, [], a
    | m :: l, FProd (_, a, b) ->
      let repo, m = obj repo env (m, a) in
      let repo, l, a = app repo env (l, Subst.fam 0 m b) in
      repo, m :: l, a
    | [], _ -> failwith "not eta-expanded"
    | _ :: _, FApp _ -> failwith "non-functional application"

  and fapp repo env : spine * kind -> unit = function
    | [], KType -> ()
    | _ :: _, KType -> failwith "non-functional application"
    | [], _ -> failwith "not eta-expanded"
    | m :: l, KProd (_, a, k) ->
      let _ = obj repo env (m, a) in
      fapp repo env (l, Subst.kind 0 m k)

  let rec fam repo env = function
    | FApp (c, l) -> fapp repo env (l, Sign.ffind c repo.sign)
    | FProd (_, a, b) -> fam repo env a; fam repo (Env.add a env) b

  let rec kind repo env = function
    | KType -> ()
    | KProd (_, a, k) -> fam repo env a; kind repo (Env.add a env) k

  let app repo env (h, l) =
    let a, _ = head repo env h in
    app repo env (l, a)

end

let rec init s = function
  | [] -> s
  | (c, t, b) :: s' ->
    let repo = { Repo.sign = s;
                 Repo.ctx = Repo.Context.empty;
                 Repo.head = Names.Meta.make "DUMMY" } in
    match LF.Strat.term s [] t, b with
      | LF.Strat.Obj _, _ -> failwith "object in sign"
      | LF.Strat.Kind _, false -> failwith "kind cannot be non-sliceable"
      | LF.Strat.Fam a, b ->
        Check.fam repo LF.Env.empty a;
        init (LF.Sign.oadd (Names.OConst.make c) (b, a) s) s'
      | LF.Strat.Kind k, true ->
        Check.kind repo LF.Env.empty k;
        init (LF.Sign.fadd (Names.FConst.make c) k s) s'

let push repo env (h, l) =
  let repo, l, a = Check.app repo env (h, l) in
  push repo env a (h, l)

open Util
open LF
open Struct

let pull repo x =
  let rec aux ctx x s =
    let e, m, a = Context.find x ctx in
    assert (List.length (Env.to_list e) = List.length s);
    let m = Subst.obj s m in
    LF.Util.map_meta (aux ctx) m
  in aux repo.Repo.ctx x []

let push =
  let gensym =
    let n = ref 0 in
    fun () -> incr n; string_of_int !n in
  fun repo env a (h, l) ->
    let x = Names.Meta.make ("X"^gensym()) in
    let repo = { repo with
      Repo.ctx = Context.add x (env, inj @@ LF.OApp (h, l), a) repo.Repo.ctx;
      Repo.head = x } in
    let s = List.map_i 0 (fun i _ -> inj @@ OApp (HVar i, [])) (Env.to_list env) in
    repo, s

let is_defined repo c = match Sign.ofind c repo.Repo.sign with
  | _, Sign.Defined f -> true
  | _ -> false

let interpret repo c l = match Sign.ofind c repo.Repo.sign with
  | _, Sign.Defined f ->
    let r = f repo l in
    Format.printf "evalué pr conv: %a = %a@." SLF.Printer.obj (inj @@ OApp (HConst c, l)) SLF.Printer.obj r;
    r
  | _ -> assert false


module Conv = struct

  exception Not_conv_obj of Repo.t * obj * obj
  exception Not_conv_fam of Repo.t * fam * fam

  let head repo = function
    | HVar i1, HVar i2 when i1 = i2 -> ()
    | HConst c1, HConst c2 when Names.OConst.compare c1 c2 = 0 -> ()
    | h1, h2 -> raise (Not_conv_obj (repo, inj @@ OApp (h1, []), inj @@ OApp (h2, [])))

  let rec spine repo = function
    | [], [] -> ()
    | m1 :: l1, m2 :: l2 -> obj repo (m1, m2); spine repo (l1, l2)
    | l1, l2 ->
      let h = HConst (Names.OConst.make "@") in
      raise (Not_conv_obj (repo, inj @@ OApp (h, l1), inj @@ OApp (h, l2)))

  and obj' repo (m1, m2) = match prj m1, prj m2 with
    | OLam (_, m1), OLam (_,m2) -> obj repo (m1, m2)
    | OApp (HConst c, l), _ when is_defined repo c -> obj repo (interpret repo c l, m2)
    | _, OApp (HConst c, l) when is_defined repo c-> obj repo (m1, interpret repo c l)
    | OApp (h1, l1), OApp (h2, l2) -> head repo (h1, h2); spine repo (l1, l2)
    | OMeta (x1, s1), OMeta (x2, s2) ->
      if Names.Meta.compare x1 x2 = 0 then spine repo (s1, s2) else
        let e1, m1, a1 = Context.find x1 repo.Repo.ctx in
        let e2, m2, a2 = Context.find x2 repo.Repo.ctx in
        assert (List.length (Env.to_list e1) = List.length s1);
        assert (List.length (Env.to_list e2) = List.length s2);
        let m1 = Subst.obj s1 m1 in
        let m2 = Subst.obj s2 m2 in
        obj repo (m1, m2)

    | (OMeta _ as m1), m2 | m1, (OMeta _ as m2) ->
      raise (Not_conv_obj (repo, inj m1, inj m2))
    | m1, m2 -> raise (Not_conv_obj (repo, inj m1, inj m2))

  and obj repo (m1, m2) =
    Format.printf "** conv %a == %a@." SLF.Printer.obj m1 SLF.Printer.obj m2;
    obj' repo (m1, m2)

  let rec fam repo = function
    | FProd (_, a1, b1), FProd (_, a2, b2) ->
      fam repo (a1, a2); fam repo (b1, b2)
    | FApp (c1, l1), FApp (c2, l2) when Names.FConst.compare c1 c2 = 0 ->
      spine repo (l1, l2)
    | a1, a2 -> raise (Not_conv_fam (repo, a1, a2))
end

module Check = struct

  exception Non_functional_fapp of Repo.t * Env.t * spine
  exception Non_functional_app of Repo.t * Env.t * spine * fam

  let head repo env : head -> fam * Sign.entry_type = function
    | HVar x ->
      let a = try Env.find x env with _ -> failwith(string_of_int x) in
      let a = Lift.fam 0 (x+1) a in
      a, Sign.Non_sliceable
    | HConst c -> Sign.ofind c repo.Repo.sign

  let rec obj' repo env : obj * fam -> Repo.t * obj = Prod.map prj id @>
    begin function
    | OLam (x, m), FProd (y, a, b) ->
      let x = match x, y with
        | None, Some _ -> x
        | _ -> x in
      let repo, m = obj repo (Env.add x a env) (m, b) in
      repo, OLam (x, m)
    | OLam _, FApp _ -> failwith "not eta"
    | OApp (h, l), a ->
      let b, e = head repo env h in
      let repo, l, a' = spine repo env (l, b) in
      Conv.fam repo (a, a');
      begin match e with
        | Sign.Sliceable ->
          let repo, s = push repo env a (h, l) in
          repo, OMeta (repo.Repo.head, s)
        | Sign.Non_sliceable ->
          repo, OApp (h, l)
        | Sign.Defined f ->
          Format.printf "eval: %a " SLF.Printer.obj (inj @@ OApp (h, l));
          let r = f repo l in
          Format.printf " = %a@." SLF.Printer.obj r;
          let _, _ = obj repo env (r, a) in
          repo, prj r
      end
    | OMeta (x, s) as m, a ->
      let e, _, b = Context.find x repo.Repo.ctx in
      let b = Subst.fam s b in
      Conv.fam repo (a, b);
      repo, m
    end @> Prod.map id inj

  and obj repo env (m, a) =
    let e = Env.names_of env in
    Format.printf "** obj @[%a@] ⊢ @[%a@] : @[%a@]@." SLF.Printer.env env
      (SLF.Printer.eobj e) m (SLF.Printer.efam e) a;
    obj' repo env (m, a)

  and spine repo env : spine * fam -> Repo.t * spine * fam = function
    | [], (FApp _ as a) -> repo, [], a
    | m :: l, FProd (_, a, b) ->
      let repo, m = obj repo env (m, a) in
      let repo, l, a = spine repo env (l, Subst.fam [m] b) in
      repo, m :: l, a
    | [], _ -> failwith "not eta-expanded"
    | _ :: _ as l, (FApp _ as a) -> raise (Non_functional_app (repo, env, l, a))

  and fspine repo env : spine * kind -> Repo.t * spine = function
    | [], KType -> repo, []
    | _ :: _ as l, KType -> raise (Non_functional_fapp (repo, env, l))
    | [], _ -> failwith "not eta-expanded"
    | m :: l, KProd (_, a, k) ->
      let repo, m = obj repo env (m, a) in
      let repo, l = fspine repo env (l, Subst.kind [m] k) in
      repo, m :: l

  let rec fam repo env : fam -> Repo.t * fam = function
    | FApp (c, l) ->
      let repo, l = fspine repo env (l, Sign.ffind c repo.Repo.sign) in
      repo, FApp (c, l)
    | FProd (x, a, b) ->
      let repo, a = fam repo env a in
      let repo, b = fam repo (Env.add x a env) b in
      repo, FProd (x, a, b)

  let rec kind repo env : kind -> Repo.t * kind = function
    | KType -> repo, KType
    | KProd (x, a, k) ->
      let repo, a = fam repo env a in
      let repo, k = kind repo (Env.add x a env) k in
      repo, KProd (x, a, k)

  let app repo env (h, l) =
    let a, _ = head repo env h in       (* TODO si c'est un defined? *)
    spine repo env (l, a)

end

let rec init repo = function
  | [] -> repo
  | (c, t, e) :: s' ->
    match SLF.Strat.term repo.Repo.sign [] t, e with
      | SLF.Strat.Fam a, e ->
        let repo, a = Check.fam repo Env.empty a in
        let e = SLF.Strat.entry_type e in
        let repo = {repo with Repo.sign = Sign.oadd (Names.OConst.make c) (a, e) repo.Repo.sign} in
        init repo s'
      | SLF.Strat.Kind k, SLF.Sliceable ->
        let repo, k = Check.kind repo Env.empty k in
        let repo = {repo with Repo.sign = Sign.fadd (Names.FConst.make c) k repo.Repo.sign} in
        init repo s'
      | SLF.Strat.Obj _, _ -> failwith "object in sign"
      | SLF.Strat.Kind _, _ -> failwith "kind cannot be non-sliceable or defined"

let push repo env (h, l) =
  let repo, l, a = Check.app repo env (h, l) in
  push repo env a (h, l)

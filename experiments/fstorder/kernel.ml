
module Check = struct

  open LF
  open Repo

  let rec obj repo env : obj * fam -> unit = function
    | OLam (x, m), FProd (_, a, b) -> obj repo (Env.add a env) (m, b)
    | OLam _, FApp _ -> failwith "not eta"
    | OApp (h, l), a ->
      let b = app repo env (l, head repo env h) in
      if a = b then () else failwith "type mismatch" (* TODO eq *)
    | OMeta x, a ->
      if snd (Repo.Context.find x repo.ctx) = a then () (* TODO eq *)
      else failwith "eq"

  and head repo env : head -> fam = function
    | HVar x -> (try Env.find x env with _ ->
      Format.printf "env: %a" Print.(pr_list pr_comma (LF.Printer.fam)) (Env.to_list env);
      failwith ("Env.find: "^string_of_int x))
    | HConst c -> Sign.ofind c repo.sign

  and app repo env : spine * fam -> fam = function
    | [], (FApp _ as a) -> a
    | [], _ -> failwith "not eta-expanded"
    | m :: l, FProd (_, a, b) ->
      obj repo env (m, a);
      app repo env (l, Subst.fam m b)
    | _ :: _, FApp _ -> failwith "non-functional application"

  and fapp repo env : spine * kind -> unit = function
    | [], KType -> ()
    | _ :: _, KType -> failwith "non-functional application"
    | [], _ -> failwith "not eta-expanded"
    | m :: l, KProd (_, a, k) ->
      obj repo env (m, a);
      fapp repo env (l, Subst.kind m k)

  let rec fam repo env = function
    | FApp (c, l) -> fapp repo env (l, Sign.ffind c repo.sign)
    | FProd (_, a, b) -> fam repo env a; fam repo (Env.add a env) b

  let rec kind repo env = function
    | KType -> ()
    | KProd (_, a, k) -> fam repo env a; kind repo (Env.add a env) k

  let app repo env (h, l) = app repo env (l, head repo env h)

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

let push =
  let gensym =
    let n = ref 0 in
    fun () -> incr n; string_of_int !n in
  fun repo (h, l) ->
    Format.printf "* push %a in@.%a@." LF.Printer.obj (LF.OApp(h, l)) Repo.Printer.t_light repo;
    let a = Check.app repo LF.Env.empty (h, l) in
    let x = Names.Meta.make ("X"^gensym()) in
    let repo = { repo with
      Repo.ctx = Repo.Context.add x (LF.OApp (h, l), a) repo.Repo.ctx;
      Repo.head = x } in
    Format.printf "* push => %a@." Repo.Printer.t_light repo;
    repo

let pull repo x =
  let rec aux ctx x =
    LF.Util.fold_meta (aux ctx) (fst (Repo.Context.find x ctx))
  in aux repo.Repo.ctx x


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
    Format.printf "* push %a |- @[%a@] in @[%a@]" LF.Printer.env env LF.Printer.obj (LF.OApp(h, l)) Repo.Printer.t_light repo;
    let x = Names.Meta.make ("X"^gensym()) in
    let repo = { repo with
      Repo.ctx = Repo.Context.add x (LF.OApp (h, l), a) repo.Repo.ctx;
      Repo.head = x } in
    repo

module Check = struct

  let head repo env : head -> fam * bool = function
    | HVar x -> (try Env.find x env with _ ->
      Format.printf "env: %a" Print.(pr_list pr_comma (LF.Printer.fam)) (Env.to_list env);
      failwith ("Env.find: "^string_of_int x)),
      false
    | HConst c -> Sign.ofind c repo.sign, Sign.slices c repo.sign

  let rec obj repo env : obj * fam -> Repo.t * obj = function
    | OLam (x, m), FProd (_, a, b) ->
      let repo, m = obj repo (Env.add a env) (m, b) in
      repo, OLam (x, m)
    | OLam _, FApp _ -> failwith "not eta"
    | OApp (h, l), a ->
      let b, slices = head repo env h in
      let repo, l, a' = app repo env (l, b) in
      if a <> a' then failwith "type mismatch"; (* TODO eq *)
      if slices then
        let repo = push repo env a (h, l) in
        repo, OMeta (repo.head)
      else
        repo, OApp (h, l)
    | OMeta x as m, a ->
      if snd (Repo.Context.find x repo.ctx) <> a (* TODO eq *)
      then failwith "eq";
      repo, m

  and app repo env : spine * fam -> Repo.t * spine * fam = function
    | [], (FApp _ as a) -> repo, [], a
    | m :: l, FProd (_, a, b) ->
      let repo, m = obj repo env (m, a) in
      let repo, l, a = app repo env (l, Subst.fam m b) in
      repo, m :: l, a
    | [], _ -> failwith "not eta-expanded"
    | _ :: _, FApp _ -> failwith "non-functional application"

  and fapp repo env : spine * kind -> unit = function
    | [], KType -> ()
    | _ :: _, KType -> failwith "non-functional application"
    | [], _ -> failwith "not eta-expanded"
    | m :: l, KProd (_, a, k) ->
      let _ = obj repo env (m, a) in
      fapp repo env (l, Subst.kind m k)

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

open Util
open LF
open Struct
open Struct.Repo

let pull repo x =
  let rec aux ctx x s =
    let e, m, a = Context.find x ctx in
    assert (List.length (Env.to_list e) = List.length s);
    let m = Subst.obj s m in
    LF.Util.map_meta (aux ctx) m
  in aux repo.ctx x []

(* Γ ⊢ σ : Γ'  Γ ⊢ M ≡ M'[σ]  Γ ⊢ A ≡ A'[σ]
 * ———————————————————————————————————————— (Δ minimal for M, σ renaming)
 * (Γ ⊢ M : A) ~> (Γ' ⊢ M' : A'), σ
 *)
let strengthen env (h, l) a =
  let fv = (LF.Util.fv (mkApp(h, l))) in
  let subst = Renaming.subst_of env fv in  (* Γ ⊢ σ : Γ') *)
  let env' = Renaming.drop_env fv env in
  let subst' = Renaming.subst_of env' (Renaming.inverse fv) in (* Γ' ⊢ σ' : Γ *)
  let l' = List.map (Subst.obj subst') l in (* M'=M[σ'] *)
  let a' = Subst.fam subst' a in            (* A'=A[σ'] *)
  env', (h, l'), a', subst

let strengthen env (h, l) a =
  let e = Env.names_of env in
  Format.printf "**** strengthen %a ⊢ %a : %a@." SLF.Printer.env env (SLF.Printer.eobj e) (mkApp(h,l)) (SLF.Printer.efam e) a;
  let env', (h, l), a, subst = strengthen env (h, l) a in
  Format.printf "**** strengthen ==> %a ⊢ %a : %a, σ = (%a ⊢ %a)@." SLF.Printer.env env' (SLF.Printer.eobj (Env.names_of env')) (mkApp(h,l)) (SLF.Printer.efam (Env.names_of env')) a SLF.Printer.env env (SLF.Printer.esubst (Env.names_of env)) subst;
  env', (h, l), a, subst

(* —————————————————————————————————————— (X fresh)
 * R, Γ ⊢ h l : A => R[Γ ⊢ ?X = h l : A], id(Γ)
 *)
let push =
  let gensym =
    let n = ref 0 in
    fun () -> incr n; string_of_int !n in
  fun repo env (h, l) a ->
    let x = Names.Meta.make ("X"^gensym()) in
    let env, (h, l), a, s = strengthen env (h, l) a in
    let repo = { repo with
      ctx = Context.add x (env, mkApp (h, l), a) repo.ctx;
      head = x } in
    repo, s

let is_defined repo c = match Sign.ofind c repo.sign with
  | _, Sign.Defined f -> true
  | _ -> false

let interpret repo env c l = match Sign.ofind c repo.sign with
  | _, Sign.Defined f ->
    let r = f repo env l in
    Format.printf "evalué pr conv: %a = %a@." SLF.Printer.obj (mkApp (HConst c, l)) SLF.Printer.obj r;
    r
  | _ -> assert false


module Conv = struct

  exception Not_conv_obj of repo * env * obj * obj
  exception Not_conv_fam of repo * env * fam * fam

  let head repo env = function
    | HVar x, HVar y when x = y ->
      let a = try Env.find x env with _ -> failwith(string_of_int x) in
      Lift.fam 0 (x+1) a
    | HConst c1, HConst c2 when Names.OConst.compare c1 c2 = 0 ->
      fst (Sign.ofind c1 repo.sign)
    | h1, h2 -> raise (Not_conv_obj (repo, env, mkApp (h1, []), mkApp (h2, [])))

  let rec spine repo env = function
    | [], [], (FApp _ as a) -> a
    | m1 :: l1, m2 :: l2, FProd (x, a, b) ->
      obj repo env (m1, m2, a);
      spine repo env (l1, l2, Subst.fam [m1] b)
    | l1, l2, a ->
      let h = HConst (Names.OConst.make "@") in
      raise (Not_conv_obj (repo, env, mkApp (h, l1), mkApp (h, l2)))

  and fspine repo env = function
    | [], [], KType -> ()
    | m1 :: l1, m2 :: l2, KProd (x, a, b) ->
      obj repo env (m1, m2, a);
      fspine repo env (l1, l2, Subst.kind [m1] b)
    | l1, l2, a ->
      let h = HConst (Names.OConst.make "@") in
      raise (Not_conv_obj (repo, env, mkApp (h, l1), mkApp (h, l2)))

  and subst repo env = function
    | [], [], [] -> ()
    | m1 :: s1, m2 :: s2, (_, a) :: e ->
      subst repo env (s1, s2, e);
      obj repo env (m1, m2, Subst.fam s1 a)
    | _ -> failwith "subst"

  and obj' repo env (m1, m2, a) = match prj m1, prj m2, a with
    | OLam (_, m1), OLam (_,m2), FProd (x, a, b) -> obj repo (Env.add x a env) (m1, m2, b)
    | OApp (HConst c, l), _, a when is_defined repo c -> obj repo env (interpret repo env c l, m2, a)
    | _, OApp (HConst c, l), a when is_defined repo c-> obj repo env (m1, interpret repo env c l, a)
    | OApp (h1, l1), OApp (h2, l2), c ->
      let a = head repo env (h1, h2) in
      let a = spine repo env (l1, l2, a) in
      fam repo env (a, c)
    | OMeta (x1, s1), OMeta (x2, s2), a when Names.Meta.compare x1 x2 = 0 ->
      let e, _, a' = Context.find x1 repo.ctx in
      subst repo env (s1, s2, Env.to_list e);
      fam repo env (a, Subst.fam s1 a')
    | OMeta (x, s), m, a | m, OMeta (x, s), a ->
        let e, m', _ = Context.find x repo.ctx in
        assert (List.length (Env.to_list e) = List.length s);
        Format.printf "** subst %a %a@." SLF.Printer.obj m' SLF.Printer.subst s;
        let m' = Subst.obj s m' in
        obj repo env (inj m, m', a)
    | m1, m2, a -> raise (Not_conv_obj (repo, env, inj m1, inj m2))

  and obj repo env (m1, m2, a) =
    let e = Env.names_of env in
    Format.printf "** conv obj %a ⊢ %a == %a : %a@." SLF.Printer.env env
      (SLF.Printer.eobj e) m1 (SLF.Printer.eobj e) m2 (SLF.Printer.efam e) a;
    obj' repo env (m1, m2, a)

  and fam' repo env = function
    | FProd (x, a1, b1), FProd (_, a2, b2) ->
      fam repo env (a1, a2); fam repo (Env.add x a1 env) (b1, b2)
    | FApp (c1, l1), FApp (c2, l2) when Names.FConst.compare c1 c2 = 0 ->
      let k = Sign.ffind c1 repo.sign in
      fspine repo env (l1, l2, k)
    | a1, a2 -> raise (Not_conv_fam (repo, env, a1, a2))

  and fam repo env (a1, a2) =
    let e = Env.names_of env in
    Format.printf "** conv fam %a ⊢ %a == %a : *@." SLF.Printer.env env
      (SLF.Printer.efam e) a1 (SLF.Printer.efam e) a2;
    fam' repo env (a1, a2)

end

module Check = struct

  exception Non_functional_fapp of repo * env * spine
  exception Non_functional_app of repo * env * spine * fam

  let head repo env : head -> fam * Sign.entry_type = function
    | HVar x ->
      let a = try Env.find x env with _ -> failwith(string_of_int x) in
      let a = Lift.fam 0 (x+1) a in
      a, Sign.Non_sliceable
    | HConst c -> Sign.ofind c repo.sign

  (* obj: check mode, returns the rewritten obj and the enlarged repo *)
  let rec obj' repo env : obj * fam -> repo * obj = Prod.map prj id @>
    function

    (* R, Γ; x:A ⊢ m : B => R', m'
     * ———————————————————————————————————
     * R, Γ ⊢ λx. m : Πx:A. B => R', λx. m'
     *)
    | OLam (x, m), FProd (y, a, b) ->
      let x = match x, y with
        | None, Some _ -> x
        | _ -> x in
      let repo, m = obj repo (Env.add x a env) (m, b) in
      repo, mkLam (x, m)
    | OLam _, FApp _ -> failwith "not eta"

    (* R, Γ ⊢ h l => R', M', A'  R' ⊢ A ≡ A'
     * ————————————————————————————————————
     * R, Γ ⊢ h l : A => R', m'
     *)
    | OApp (h, l), a ->
      let repo, m, a' = app repo env (h, l) in
      Conv.fam repo env (a, a');
      repo, m

    (* R(X) = (Δ ⊢ _ : A')  R ⊢ A ≡ A'[σ]  R, Γ ⊢ σ : Δ => R', σ'
     * —————————————————————————————————————————————————————
     * R, Γ ⊢ ?X[σ] : A => R', σ'
     *)
    | OMeta (x, s), a ->
      let e, _, b = Context.find x repo.ctx in
      let repo, s = subst repo env (s, Env.to_list e) in
      let b = Subst.fam s b in
      Conv.fam repo env (a, b);
      repo, mkMeta (x, s)

  and obj repo env (m, a) =
    let e = Env.names_of env in
    Format.printf "** obj @[%a@] ⊢ @[%a@] : @[%a@]@." SLF.Printer.env env
      (SLF.Printer.eobj e) m (SLF.Printer.efam e) a;
    obj' repo env (m, a)

  and subst repo env : subst * ('a * fam) list -> repo * subst = function

  (* R, Γ ⊢ m : A[σ] => R', m'  R', Γ ⊢ σ : Δ => R'', σ'
   * —————————————————————————————————————————————————
   * R, Γ ⊢ σ; m : Δ; (x:A) => R'', σ'; m'
   *)
    | m :: s, (_, a) :: e ->
      let repo, s = subst repo env (s, e) in
      let repo, m = obj repo env (m, Subst.fam s a) in
      repo, m :: s

  (* ————————————————————
   * R, Γ ⊢ · : · => R, ·
   *)
    | [], [] -> repo, []
    | _ -> failwith "subst"

  and app repo env (h, l) : repo * obj * fam =
    let a, e = head repo env h in
    match e with

        (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => R', l', C
         * R', Γ ⊢ h l : C => R'', σ (push)
         * ——————————————————————————————————————— (h sliceable)
         * R, Γ ⊢ h l => R'', ?X[s], C
         *)
      | Sign.Sliceable ->
        let repo, l, a = spine repo env (l, a) in
        let repo, s = push repo env (h, l) a in
        repo, mkMeta (repo.head, s), a

      (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => R', l', C
       * ——————————————————————————————————————— (h non sliceable)
       * R, Γ ⊢ h l => R', h l', C
       *)
      | Sign.Non_sliceable ->
        let repo, l, a = spine repo env (l, a) in
        repo, mkApp (h, l), a

      (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => _, _, C
       * R ⊢ f l ~~> m
       * R, Γ ⊢ m : C => R', m'
       * ——————————————————————————————————————— (h defined = f)
       * R, Γ ⊢ h l => R', m', C
       *)
      | Sign.Defined f ->
        (* check that arguments of this constants are well-typed *)
        let _, _, a = spine repo env (l, a) in (* TODO et si A dépend du repo ignoré? *)
        Format.printf "*** eval: %a ⊢ %a...@." SLF.Printer.env env (SLF.Printer.eobj (Env.names_of env)) (mkApp (h, l));
        (* evaluate it *)
        let m = f repo env l in
        Format.printf "*** eval: %a ⊢ %a = %a@." SLF.Printer.env env (SLF.Printer.eobj (Env.names_of env)) (mkApp (h, l)) SLF.Printer.obj m;
        (* check that the result is well-typed, and take the result into account *)
        let repo, m = obj repo env (m, a) in
        repo, m, a

  and spine repo env : spine * fam -> repo * spine * fam = function

    (* ———————————————————————
     * R, Γ, P ⊢ · => R, ·, P
     *)
    | [], (FApp _ as a) -> repo, [], a

    (* R, Γ ⊢ m : A => R', m'
     * R', Γ, B[x/m'] ⊢ l => R, l', C
     * ————————————————————————————————————
     * R, Γ, Πx:A. B ⊢ m; l => R, m'; l', C
     *)
    | m :: l, FProd (_, a, b) ->
      let repo, m = obj repo env (m, a) in
      let repo, l, a = spine repo env (l, Subst.fam [m] b) in
      repo, m :: l, a
    | [], _ -> failwith "not eta-expanded"
    | _ :: _ as l, (FApp _ as a) -> raise (Non_functional_app (repo, env, l, a))

  and fspine repo env : spine * kind -> repo * spine = function
    | [], KType -> repo, []
    | _ :: _ as l, KType -> raise (Non_functional_fapp (repo, env, l))
    | [], _ -> failwith "not eta-expanded"
    | m :: l, KProd (_, a, k) ->
      let repo, m = obj repo env (m, a) in
      let repo, l = fspine repo env (l, Subst.kind [m] k) in
      repo, m :: l

  let rec fam repo env : fam -> repo * fam = function
    | FApp (c, l) ->
      let repo, l = fspine repo env (l, Sign.ffind c repo.sign) in
      repo, FApp (c, l)
    | FProd (x, a, b) ->
      let repo, a = fam repo env a in
      let repo, b = fam repo (Env.add x a env) b in
      repo, FProd (x, a, b)

  let rec kind repo env : kind -> repo * kind = function
    | KType -> repo, KType
    | KProd (x, a, k) ->
      let repo, a = fam repo env a in
      let repo, k = kind repo (Env.add x a env) k in
      repo, KProd (x, a, k)

end

let rec init repo = function
  | [] -> repo
  | (c, t, e) :: s' ->
    match SLF.Strat.term repo.sign [] t, e with
      | SLF.Strat.Fam a, e ->
        let repo, a = Check.fam repo Env.empty a in
        let e = SLF.Strat.entry_type e in
        let repo = {repo with sign = Sign.oadd (Names.OConst.make c) (a, e) repo.sign} in
        init repo s'
      | SLF.Strat.Kind k, SLF.Sliceable ->
        let repo, k = Check.kind repo Env.empty k in
        let repo = {repo with sign = Sign.fadd (Names.FConst.make c) k repo.sign} in
        init repo s'
      | SLF.Strat.Obj _, _ -> failwith "object in sign"
      | SLF.Strat.Kind _, _ -> failwith "kind cannot be non-sliceable or defined"

let push repo env (h, l) =
  Format.printf "** push %a ⊢ %a@." SLF.Printer.env env (SLF.Printer.eobj (Env.names_of env)) (inj (OApp(h, l)));
  let repo, m, a = Check.app repo env (h, l) in
  Format.printf "** push => %a in @[%a@]@." SLF.Printer.obj m SLF.Printer.repo_light repo;
  match prj m with
    | OApp (h, l) -> push repo env (h, l) a
    | OMeta (x, s) -> {repo with head = x}, s
    | OLam _ -> assert false

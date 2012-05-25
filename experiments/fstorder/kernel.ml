open Util
open Names
open LF
open Struct
open Struct.Repo

module P = SLF.Printer

exception Not_conv_obj of repo * env * obj * obj * fam option
exception Not_conv_fam of repo * env * fam * fam
exception Non_functional_fapp of repo * env * spine
exception Non_functional_app of repo * env * spine * fam
exception Non_functional_obj of repo * env * obj * fam
exception Unbound_meta of repo * Meta.t
exception Unbound_variable of repo * env * int
exception Not_evaluable of repo * SLF.term
exception Not_eta_expanded of repo * env * spine * (fam, kind) union

let _ =
  Topcatch.register begin fun fmt -> function
    | LF.Not_eta (m, s) -> Format.fprintf fmt "Not eta-expanded:@ %a@ [%a]" P.obj m P.spine s
    | Not_conv_obj (repo, env, m1, m2, a) -> Format.fprintf fmt "Not convertible:@ @[%a ⊢ %a ≡ %a : %a@] in %a"
      P.env env P.obj m1 P.obj m2 (Print.opt_under P.fam) a P.repo_light repo
    | Not_conv_fam (repo, env, m1, m2) -> Format.fprintf fmt "Not convertible:@ @[%a ⊢ %a ≡ %a@] in %a"
      P.env env P.fam m1 P.fam m2 P.repo_light repo
    | Non_functional_fapp (repo, env, l) ->
        let e = Env.names_of env in
        Format.fprintf fmt "Non functional:@ @[%a ⊢ %a : *@]"
      P.env env (P.espine e) l
    | Non_functional_app (repo, env, l, a) ->
        let e = Env.names_of env in
        Format.fprintf fmt "Non functional:@ @[%a ⊢ %a : %a@]"
          P.env env (P.espine e) l P.fam a
    | Non_functional_obj (repo, env, m, a) ->
        let e = Env.names_of env in
        Format.fprintf fmt "Non functional:@ @[%a ⊢ %a : %a@] in %a"
          P.env env (P.eobj e) m (P.efam e) a P.repo_light repo
    | Unbound_meta (repo, x) -> Format.fprintf fmt "Unbound meta:@ %a in %a" Meta.print x P.repo_light repo
    | Not_evaluable (repo, t) -> Format.fprintf fmt "Not evaluable:@ %a in %a" P.term t P.repo_light repo
    | Not_eta_expanded (repo, env, s, a) -> Format.fprintf fmt "Not eta-expanded:@ %a, %a ⊢ %a" P.env env (Print.union P.fam P.kind) a P.subst s
    | _ -> raise Topcatch.Unhandled
  end

let pull repo x =
  let rec aux ctx (x, s) =
    let e, m, a =
      try Context.find x ctx
      with Not_found -> raise (Unbound_meta (repo, x)) in
    assert (List.length e = List.length s);
    let m = Subst.obj s m in
    LF.Util.map_meta (aux ctx) m
  in aux repo.ctx x

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

(* let strengthen env (h, l) a = *)
(*   let e = Env.names_of env in *)
(*   Debug.log_open "strengthen" "%a ⊢ %a : %a" P.env env (P.eobj e) (mkApp(h,l)) (P.efam e) a; *)
(*   let env', (h, l), a, subst = strengthen env (h, l) a in *)
(*   Debug.log_close "strengthen" "=> %a ⊢ %a : %a, σ = (%a ⊢ %a)" P.env env' (P.eobj (Env.names_of env')) (mkApp(h,l)) (P.efam (Env.names_of env')) a P.env env (P.esubst (Env.names_of env)) subst; *)
(*   env', (h, l), a, subst *)

(* —————————————————————————————————————— (X fresh)
 * R, Γ ⊢ h l : A => R[?X = Γ ⊢ h l : A], id(Γ)
 *)
let push repo env (h, l) a =
    let x, repo = Repo.fresh repo in
    let env, (h, l), a, s = strengthen env (h, l) a in
    let repo = { repo with ctx = Context.add x (env, mkApp (h, l), a) repo.ctx } in
    repo, (x, s)

let push repo env (h, l) a =
  let e = Env.names_of env in
  Debug.log_open "push" "%a ⊢ %a : %a" P.env env (P.eobj e) (mkApp(h,l)) (P.efam e) a;
  let r, (x, s) = push repo env (h, l) a in
  Debug.log_close "push" "=> %a, %a%a" P.repo_light r Meta.print x P.subst s;
  r, (x, s)

module rec Conv : sig
  val obj : repo -> env -> obj * obj * fam -> unit
  val fam : repo -> env -> (fam * fam) -> unit
end = struct

  exception Subst_not_conv

  let head repo env = function
    | HVar x, HVar y when x = y ->
      let a = try Env.find x env with _ -> raise (Unbound_variable (repo, env, x)) in
      Lift.fam 0 (x+1) a
    | HConst c1, HConst c2 when OConst.compare c1 c2 = 0 ->
      fst (Sign.ofind c1 repo.sign)
    | h1, h2 -> raise (Not_conv_obj (repo, env, mkApp (h1, []), mkApp (h2, []), None))

  let rec spine repo env h = function
    | [], [], (FApp _ as a) -> a
    | m1 :: l1, m2 :: l2, FProd (x, a, b) ->
        let s = spine repo env h (l1, l2, Subst.fam [m1] b) in
        obj repo env (m1, m2, a);
        s
    | l1, l2, a -> raise (Not_conv_obj (repo, env, mkApp (h, l1), mkApp (h, l2), Some a))

  and fspine repo env = function
    | [], [], KType -> ()
    | m1 :: l1, m2 :: l2, KProd (x, a, b) ->
        let s = fspine repo env (l1, l2, Subst.kind [m1] b) in
        obj repo env (m1, m2, a);
        s
    | l1, l2, a ->
      let h = HConst (OConst.make "@") in
      raise (Not_conv_obj (repo, env, mkApp (h, l1), mkApp (h, l2), None))

  and subst repo env = function
    | [], [], [] -> ()
    | m1 :: s1, m2 :: s2, (_, a) :: e ->
      subst repo env (s1, s2, e);
      obj repo env (m1, m2, Subst.fam s1 a)
    | _ -> raise Subst_not_conv

  and obj' repo env (m1, m2, a) = match prj m1, prj m2, a with
    | OLam (x, m1), OLam (y,m2), FProd (z, a, b) ->
        let x = match x, y, z with
          | None, None, Some _ -> z
          | None, Some _, _ -> y
          | _ -> x in
        obj repo (Env.add x a env) (m1, m2, b)
    | OMeta (x1, s1), OMeta (x2, s2), a when Meta.compare x1 x2 = 0 ->
        let e, _, a' =
          try Context.find x1 repo.ctx
          with Not_found -> raise (Unbound_meta (repo, x1)) in
        begin try subst repo env (s1, s2, e)
        with Subst_not_conv -> raise (Not_conv_obj (repo, env, m1, m2, Some a)) end;
        fam repo env (a, Subst.fam s1 a')
    | OMeta (x, s), m, a | m, OMeta (x, s), a ->
        let e, m', _ =
        try Context.find x repo.ctx
        with Not_found -> raise (Unbound_meta (repo, x)) in
        assert (List.length e = List.length s);
        Debug.log "subst" "%a %a" P.obj m' P.subst s;
        let m' = Subst.obj s m' in
        obj repo env (inj m, m', a)
    | OApp (HInv (c, n), l), _, a ->
        obj repo env (List.nth l n, m2, a)
    | _, OApp (HInv (c, n), l), a ->
        obj repo env (m1, List.nth l n, a)
    | OApp (h1, l1), o2, a ->
        begin match Check.head repo env h1 with
          | ah, Sign.Defined f ->
              (* there should be no defined constants during conversion anymore *)
              assert false
          | ah, Sign.Sliceable | ah, Sign.Non_sliceable ->
              match o2 with
                | OMeta _ | OLam _ -> raise (Not_conv_obj (repo, env, m1, m2, Some a))
                | OApp (h2, l2) ->
                    match Check.head repo env h2 with
                      | ah, Sign.Defined f -> assert false
                      | ah, Sign.Sliceable | ah, Sign.Non_sliceable ->
                          let a' = head repo env (h1, h2) in
                          let a' = spine repo env h1 (l1, l2, a') in
                          fam repo env (a, a')
        end
    | m1, m2, a -> raise (Not_conv_obj (repo, env, inj m1, inj m2, Some a))

  and obj repo env (m1, m2, a) =
    let e = Env.names_of env in
    Debug.log_open "conv obj" "%a ⊢ %a ≡ %a : %a" P.env env (P.eobj e) m1 (P.eobj e) m2 (P.efam e) a;
    let r = obj' repo env (m1, m2, a) in
    Debug.close "conv obj";
    r

  and fam repo env = function
    | FProd (x, a1, b1), FProd (_, a2, b2) ->
      fam repo env (a1, a2); fam repo (Env.add x a1 env) (b1, b2)
    | FApp (c1, l1), FApp (c2, l2) when FConst.compare c1 c2 = 0 ->
      let k = Sign.ffind c1 repo.sign in
      fspine repo env (l1, l2, k)
    | a1, a2 -> raise (Not_conv_fam (repo, env, a1, a2))

  (* and fam repo env (a1, a2) = *)
  (*   let e = Env.names_of env in *)
  (*   Debug.log_open "conv fam" "%a ⊢ %a ≡ %a" P.env env (P.efam e) a1 (P.efam e) a2; *)
  (*   let r = fam' repo env (a1, a2) in *)
  (*   Debug.close "conv fam"; *)
  (*   r *)

end

and Check : sig
  val fam : red:bool -> repo -> env -> fam -> repo * fam
  val head : repo -> env -> head -> fam * Sign.entry_type
  val kind : red:bool -> repo -> env -> kind -> repo * kind
  val app : red:bool -> repo -> env -> head * spine -> repo * obj * fam
  val spine : red:bool -> repo -> env -> spine * fam -> repo * spine * fam
  val obj : red:bool -> repo -> env -> obj * fam -> repo * obj
end = struct

  exception Subst_mismatch

  let head repo env : head -> fam * Sign.entry_type = function
    | HVar x ->
        let a = try Env.find x env with _ -> raise (Unbound_variable (repo, env, x)) in
        let a = Lift.fam 0 (x+1) a in
        a, Sign.Non_sliceable
    | HConst c -> Sign.ofind c repo.sign
    | HInv (c, n) ->
      match Sign.ofind c repo.sign with
        | a, Sign.Defined _ -> LF.Util.inv_fam (n, a), Sign.Non_sliceable
        | _ -> failwith ("inverted a non-defined function")

  (* obj: check mode, returns the rewritten obj and the enlarged repo *)
  let rec obj' ~red repo env : obj * fam -> repo * obj = Prod.map prj id @>
    function

    (* R, Γ; x:A ⊢ m : B => R', m'
     * ———————————————————————————————————
     * R, Γ ⊢ λx. m : Πx:A. B => R', λx. m'
     *)
    | OLam (x, m), FProd (y, a, b) ->
      let x = match x, y with
        | None, Some _ -> y
        | _ -> x in
      let repo, m = obj ~red repo (Env.add x a env) (m, b) in
      repo, mkLam (x, m)
    | (OLam _ as m), (FApp _ as a) -> raise (Non_functional_obj (repo, env, inj m, a))

    (* R, Γ ⊢ h l => R', M', A'  R' ⊢ A ≡ A'
     * ————————————————————————————————————
     * R, Γ ⊢ h l : A => R', m'
     *)
    | OApp (h, l), a ->
      let repo, m, a' = app ~red repo env (h, l) in
      Conv.fam repo env (a, a');
      repo, m

    (* R(X) = (Δ ⊢ _ : A')  R ⊢ A ≡ A'[σ]  R, Γ ⊢ σ : Δ => R', σ'
     * —————————————————————————————————————————————————————
     * R, Γ ⊢ ?X[σ] : A => R', ?X[σ']
     *)
    | OMeta (x, s), a ->
      let e, _, b =
        try Context.find x repo.ctx
        with Not_found -> raise (Unbound_meta (repo, x)) in
      let repo, s =
        try subst ~red repo env (s, e)
        with Subst_mismatch -> raise (Non_functional_obj (repo, env, mkMeta(x, s), a)) in
      let b = Subst.fam s b in
      Conv.fam repo env (a, b);
      repo, mkMeta (x, s)

  and obj ~red repo env (m, a) =
    let e = Env.names_of env in
    Debug.log_open "obj" "%a ⊢ %a : %a" P.env env (P.eobj e) m (P.efam e) a;
    let r = obj' ~red repo env (m, a) in
    Debug.close "obj";
    r

  and subst ~red repo env : subst * ('a * fam) list -> repo * subst = function

  (* R, Γ ⊢ m : A[σ] => R', m'  R', Γ ⊢ σ : Δ => R'', σ'
   * —————————————————————————————————————————————————
   * R, Γ ⊢ σ; m : Δ; (x:A) => R'', σ'; m'
   *)
    | m :: s, (_, a) :: e ->
      let repo, s = subst ~red repo env (s, e) in
      let repo, m = obj ~red repo env (m, Subst.fam s a) in
      repo, m :: s

  (* ————————————————————
   * R, Γ ⊢ · : · => R, ·
   *)
    | [], [] -> repo, []
    | _ -> raise Subst_mismatch

  and app ~red repo env (h, l) : repo * obj * fam =
    let a, e = head repo env h in
    match e with

        (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => R', l', C
         * R', Γ ⊢ h l : C => R'', σ (push)
         * ——————————————————————————————————————— (h sliceable)
         * R, Γ ⊢ h l => R'', ?X[s], C
         *)
      | Sign.Sliceable ->
        let repo, l, a = spine ~red repo env (l, a) in
        let repo, hd = push repo env (h, l) a in
        repo, mkMeta hd, a

      (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => R', l', C
       * ——————————————————————————————————————— (h non sliceable)
       * R, Γ ⊢ h l => R', h l', C
       *)
      | Sign.Non_sliceable ->
        let repo, l, a = spine ~red repo env (l, a) in
        repo, mkApp (h, l), a

      (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => _, _, C
       * R ⊢ f l ~~> m
       * R, Γ ⊢ m : C => R', m'
       * ——————————————————————————————————————— (h defined = f)
       * R, Γ ⊢ h l => R', m', C
       *)
      | Sign.Defined f ->
        (* check and reduce arguments of this constant *)
        let repo, l, a = spine ~red repo env (l, a) in
        (* if red, evaluate it with the reduced arguments *)
        let repo, m = if red then Eval.interp repo env h f l else repo, mkApp(h, l) in
        (* check that the result is well-typed, and take the result into account *)
        let repo, m = obj ~red repo env (m, a) in
        repo, m, a

  and spine' ~red repo env : spine * fam -> repo * spine * fam = function

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
      let repo, m = obj ~red repo env (m, a) in
      let repo, l, a = spine ~red repo env (l, Subst.fam [m] b) in
      repo, m :: l, a
    | [], a -> raise (Not_eta_expanded (repo, env, [], Inl a))
    | _ :: _ as l, (FApp _ as a) -> raise (Non_functional_app (repo, env, l, a))

  and spine ~red repo env (l, a) =
    (* Debug.log_open "spine" "%a, %a ⊢ %a" P.env env P.fam a P.spine l; *)
    let repo, l, a = spine' ~red repo env (l, a) in
    (* Debug.log_close "spine" "=> %a : %a" P.spine l P.fam a; *)
    repo, l, a

  and fspine ~red repo env : spine * kind -> repo * spine = function
    | [], KType -> repo, []
    | _ :: _ as l, KType -> raise (Non_functional_fapp (repo, env, l))
    | [], k -> raise (Not_eta_expanded (repo, env, [], Inr k))
    | m :: l, KProd (_, a, k) ->
      let repo, m = obj ~red repo env (m, a) in
      let repo, l = fspine ~red repo env (l, Subst.kind [m] k) in
      repo, m :: l

  let rec fam ~red repo env : fam -> repo * fam = function
    | FApp (c, l) ->
      let repo, l = fspine ~red repo env (l, Sign.ffind c repo.sign) in
      repo, FApp (c, l)
    | FProd (x, a, b) ->
      let repo, a = fam ~red repo env a in
      let repo, b = fam ~red repo (Env.add x a env) b in
      repo, FProd (x, a, b)

  let rec kind ~red repo env : kind -> repo * kind = function
    | KType -> repo, KType
    | KProd (x, a, k) ->
      let repo, a = fam ~red repo env a in
      let repo, k = kind ~red repo (Env.add x a env) k in
      repo, KProd (x, a, k)

end

and Eval : sig
  val interp : repo -> env -> head
    -> (repo -> env -> (repo -> env -> obj -> repo * obj) -> spine -> repo * obj)
    -> spine -> repo * obj
end = struct

  let rec eval' repo env = prj @> function
    | OMeta (x, s) ->
        let e, m, _ =
          try Context.find x repo.ctx
          with Not_found -> raise (Unbound_meta (repo, x)) in
        assert (List.length e = List.length s);
        repo, Subst.obj s m
    | OApp (h, l) ->
        begin match Check.head repo env h with
          | a, Sign.Defined f ->
              let repo, l, a = Check.spine ~red:false repo env (l, a) in
              let repo, m = interp repo env h f l in
              eval repo env m
          | _ -> repo, mkApp(h, l)
        end
    | m -> repo, inj m

  and eval repo env m =
    Debug.log_open "eval" "%a" P.obj m;
    let repo, m = eval' repo env m in
    Debug.log_close "eval" "=> %a" P.obj m;
    repo, m

  and interp' repo env h (f : repo -> env -> (repo -> env -> obj -> repo * obj) -> spine -> repo * obj) l =
    let c = match h with HConst c -> c | _ -> assert false in
    match List.map prj l with
      | [OApp (HInv (c', 0), [m1; m2])] when OConst.compare c c' = 0 ->
            (* if the only argument is the inverse function c^0 *)
          repo, m2
      | _ -> f repo env eval l

  and interp repo env h f l =
    Debug.log_open "interp" "%a" P.obj (mkApp (h, l));
    let repo, m = interp' repo env h f l in
    Debug.log_close "interp" "=> %a = %a" P.obj (mkApp (h, l)) P.obj m;
    repo, m

end

let rec init repo = function
  | [] -> repo
  | (c, t, e) :: s' ->
    match SLF.Strat.term repo.sign [] t, e with
      | SLF.Strat.Fam a, e ->
        let repo, a = Check.fam false repo [] a in
        let e = SLF.Strat.entry_type e in
        let repo = {repo with sign = Sign.oadd (OConst.make c) (a, e) repo.sign} in
        init repo s'
      | SLF.Strat.Kind k, SLF.Sliceable ->
        let repo, k = Check.kind false repo [] k in
        let repo = {repo with sign = Sign.fadd (FConst.make c) k repo.sign} in
        init repo s'
      | SLF.Strat.Obj _, _ -> failwith "object in sign"
      | SLF.Strat.Kind _, _ -> failwith "kind cannot be non-sliceable or defined"

let init repo s =
  Debug.log_open "sign" "begin";
  let r = init repo s in
  Debug.log_close "sign" "end";
  r

let push repo env (h, l) =
  let repo, m, a = Check.app ~red:true repo env (h, l) in
  match prj m with
    | OApp (h, l) ->
      let repo, (x, s) = push repo env (h, l) a in
      {repo with head = x, s }
    | OMeta (x, s) -> {repo with head = x, s}
    | OLam _ -> assert false

let push repo env (h, l) =
  let e = Env.names_of env in
  Debug.log_open "push" "%a ⊢ %a" P.env env (P.eobj e) (mkApp(h, l));
  let repo = push repo env (h, l) in
  Debug.log_close "push" "%a = %a" (P.eobj e) (mkApp(h, l)) P.repo_light repo;
  repo

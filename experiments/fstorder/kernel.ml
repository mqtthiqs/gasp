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

(* —————————————————————————————————————— (X fresh)
 * R, Γ ⊢ h l : A => R[Γ ⊢ ?X : A], id(Γ)
 *)
let push =
  let gensym =
    let n = ref 0 in
    fun () -> incr n; string_of_int !n in
  fun repo env a (h, l) ->
    let x = Names.Meta.make ("X"^gensym()) in
    let repo = { repo with
      ctx = Context.add x (env, inj @@ LF.OApp (h, l), a) repo.ctx;
      head = x } in
    let s = List.map_i 0 (fun i _ -> inj @@ OApp (HVar i, [])) (Env.to_list env) in
    repo, s

let is_defined repo c = match Sign.ofind c repo.sign with
  | _, Sign.Defined f -> true
  | _ -> false

let interpret repo c l = match Sign.ofind c repo.sign with
  | _, Sign.Defined f ->
    let r = f repo l in
    Format.printf "evalué pr conv: %a = %a@." SLF.Printer.obj (inj @@ OApp (HConst c, l)) SLF.Printer.obj r;
    r
  | _ -> assert false


module Conv = struct

  exception Not_conv_obj of repo * obj * obj
  exception Not_conv_fam of repo * fam * fam

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
    | OMeta (x1, s1), OMeta (x2, s2) when Names.Meta.compare x1 x2 = 0 -> spine repo (s1, s2)
    | OMeta (x, s), m | m, OMeta (x, s) ->
        let e, m', _ = Context.find x repo.ctx in
        assert (List.length (Env.to_list e) = List.length s);
        let m' = Subst.obj s m' in
        obj repo (inj m, m')
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
      repo, inj @@ OLam (x, m)
    | OLam _, FApp _ -> failwith "not eta"

    (* R, Γ ⊢ h l => R', M', A'  R' ⊢ A ≡ A'
     * ————————————————————————————————————
     * R, Γ ⊢ h l : A => R', m'
     *)
    | OApp (h, l), a ->
      let repo, m, a' = app repo env (h, l) in
      Conv.fam repo (a, a');
      repo, m

    (* R(X) = (Δ ⊢ _ : A')  R ⊢ A ≡ A'[σ]  R, Γ ⊢ σ : Δ => R', σ'
     * —————————————————————————————————————————————————————
     * R, Γ ⊢ ?X[σ] : A => R', σ'
     *)
    | OMeta (x, s), a ->
      let e, _, b = Context.find x repo.ctx in
      let repo, s = subst repo env (s, Env.to_list e) in
      let b = Subst.fam s b in
      Conv.fam repo (a, b);
      repo, inj @@ OMeta (x, s)

  and obj repo env (m, a) =
    let e = Env.names_of env in
    Format.printf "** obj @[%a@] ⊢ @[%a@] : @[%a@]@." SLF.Printer.env env
      (SLF.Printer.eobj e) m (SLF.Printer.efam e) a;
    obj' repo env (m, a)

  and subst repo env : subst * ('a * fam) list -> repo * subst = function

  (* R, Γ ⊢ m : A => R', m'  R', Γ ⊢ σ : Δ => R'', σ'
   * —————————————————————————————————————————————————
   * R, Γ ⊢ σ; m : Δ; (x:A) => R'', σ'; m'
   *)
    | m :: s, (_, a) :: e ->
      let repo, m = obj repo env (m, a) in
      let repo, s = subst repo env (s, e) in
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
        let repo, s = push repo env a (h, l) in
        repo, inj @@ OMeta (repo.head, s), a

      (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => R', l', C
       * ——————————————————————————————————————— (h non sliceable)
       * R, Γ ⊢ h l => R', h l', C
       *)
      | Sign.Non_sliceable ->
        let repo, l, a = spine repo env (l, a) in
        repo, inj @@ OApp (h, l), a

      (* R, Γ ⊢ h => A  R, Γ, A ⊢ l => _, _, C
       * R ⊢ f l ~~> m
       * R, Γ ⊢ m : C => R', m'
       * ——————————————————————————————————————— (h defined = f)
       * R, Γ ⊢ h l => R', m', C
       *)
      | Sign.Defined f ->
        (* check that arguments of this constants are well-typed *)
        let _, _, a = spine repo env (l, a) in (* TODO et si A dépend du repo ignoré? *)
        Format.printf "*** eval: %a...@." SLF.Printer.obj (inj @@ OApp (h, l));
        (* evaluate it *)
        let m = f repo l in
        Format.printf "*** eval: %a = %a@." SLF.Printer.obj (inj @@ OApp (h, l)) SLF.Printer.obj m;
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
  Format.printf "** push %a@." SLF.Printer.obj (inj (OApp(h, l)));
  let repo, m, a = Check.app repo env (h, l) in
  Format.printf "** push => %a in @[%a@]@." SLF.Printer.obj m SLF.Printer.repo_light repo;
  match prj m with
    | OApp (h, l) -> push repo env a (h, l)
    | OMeta (x, s) -> {repo with head = x}, s
    | OLam _ -> assert false

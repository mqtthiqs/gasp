open Util

module Constants = struct
  open Name

  let commit_const = mk_fconst Settings.commit_const
  let commit_type = NLF.FAtom(Varmap.empty, commit_const, [])
  let version_const = mk_fconst Settings.version_const
  let version_type = Varmap.empty, version_const, []
  let version_o_const = mk_oconst Settings.version_o_const
  let version_o = NLF.Obj(Varmap.empty, NLF.VHead(Cst(version_o_const), (Varmap.empty, version_const, [])))
  let version_s v c = NLF.DAtom(Cst(mk_oconst Settings.version_s_const), [c; v], version_type)

end

type t = {
  sign : NLF_Sign.t;
  term : NLF.obj;
  varno : int
}

let rec compile_sign s : NLF_Sign.t = List.fold_left
  (fun sign (x,t) -> match SLF_LF.term sign t with
    | LF.Kind k ->
      let k = LF_XLF.kind k in
      let k = XLF_XLFf.kind k in
      Util.if_debug (fun () -> Format.printf "@[F %s :: %a@]@." x XLFf_Pp.kind k);
      let k = XLFf_NLF.kind sign Constants.version_o k in
      Util.if_debug (fun () -> Format.printf "@[N %s :: %a@]@." x NLF_Pp.kind k);
      NLF_Sign.add_fconst (Name.mk_fconst x) k sign
    | LF.Fam a ->
      let a = LF_XLF.fam a in
      let a = XLF_XLFf.fam a in
      Util.if_debug (fun () -> Format.printf "@[F %s :: %a@]@." x XLFf_Pp.fam a);
      let a = XLFf_NLF.fam sign Constants.version_o a in
      Util.if_debug (fun () -> Format.printf "@[N %s :: %a@]@." x NLF_Pp.fam a);
      NLF_Sign.add_oconst (Name.mk_oconst x) a sign
    | LF.Obj t -> failwith ("obj in signature: "^x)
  ) NLF_Sign.empty s

let compile_term sign repo =
    (fun x -> match SLF_LF.term sign x with
       | LF.Obj t -> t
       | _ -> assert false) //
      LF_XLF.obj //
      XLF_XLFf.obj //
      (fun t ->
	Util.if_debug (fun () -> Format.printf "%a@." XLFf_Pp.obj t);
	XLFf_NLF.obj sign repo (t, Constants.commit_type))

let reify_term t =
  (NLF_XLF.obj // LF_XLF.from_obj // SLF_LF.from_obj) t

let reify_sign s =
  NLF_Sign.fold (fun entry acc -> match entry with
    | NLF_Sign.FDecl (c, k) -> (Name.of_fconst c, SLF_LF.from_kind (LF_XLF.from_kind (NLF_XLF.kind k))) :: acc
    | NLF_Sign.ODecl (c, a) -> (Name.of_oconst c, SLF_LF.from_fam (LF_XLF.from_fam (NLF_XLF.fam a))) :: acc
  ) s []

let init sign =
  let sign = compile_sign sign in
  {sign = sign;
   varno = Name.gen_status();
   term = Constants.version_o}

let check repo =			(* TODO temp *)
  let s = reify_sign repo.sign in
  Format.printf "%a@." SLF_Pp.sign s;
  let s = compile_sign s in
  let t = reify_term repo.term in
  ignore (compile_term s Constants.version_o t)


let commit repo term =
  let old_head = match repo.term with
    | NLF.Obj(_, (NLF.VHead(h, p) as v)) -> v
    | _ -> assert false in              (* because expected type was an atom, not a product *)
  let new_term = compile_term repo.sign repo.term term in
  let new_term = match new_term with
    | NLF.Obj(sigma, (NLF.VHead(h, p) as new_head)) ->
      let x = Name.gen_variable () in
      let d = Constants.version_s old_head new_head in
      NLF.Obj(Name.Varmap.add x d sigma, NLF.VHead(Name.Var x, Constants.version_type))
    | _ -> assert false in		(* because expected type was an atom, not a product *)
  {repo with term = new_term; varno = Name.gen_status()}

let show repo = 
  Format.printf " signature:@.";
  SLF_Pp.sign Format.std_formatter (reify_sign repo.sign);
  Format.printf " term:@.";
  Format.printf "@[%a@]@." NLF_Pp.obj repo.term

let checkout repo =
  Format.printf "@[%a@]@." SLF_Pp.term (reify_term repo.term)

let load () = 
  let ch = open_in_bin !Settings.repo in
  let repo = Marshal.from_channel (open_in_bin !Settings.repo) in
  close_in ch;
  Name.gen_init repo.varno;
  repo

let save repo = 
  let ch = open_out_bin !Settings.repo in
  Marshal.to_channel ch repo [];
  close_out ch

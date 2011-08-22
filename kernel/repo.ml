open Util
open NLF
open Utils
open Definitions
open ILF

module Constants = struct
  open Name

  let commit_const = mk_fconst Settings.commit_const
  let commit_type = NLF.fconst commit_const

  let version_const = mk_fconst Settings.version_const
  let version_type = NLF.fconst version_const

  let version_o_const = mk_oconst Settings.version_o_const
  let version_o = NLF.oconst version_o_const
  let version_s_const = mk_oconst Settings.version_s_const
  let version_s = NLF.oconst version_s_const
  let version_s c v = NLF.OApp (HConst version_s_const, [c; v])

end

type t = {
  sign : NLF.signature;
  env : NLF.env;
  head : NLF.obj;
  name_status : int;
}

let compile_lf_fam sign fam =
  TypeCheck.fam sign (NLF.Environment.empty ()) (Flatten.fam (SpineForm.fam fam))

let compile_lf_kind sign kind =
  TypeCheck.kind sign (NLF.Environment.empty ()) (Flatten.kind (SpineForm.kind kind))

let compile_sign s = 
  let compile_entry outs (x, t) = 
    match SLF_LF.term outs t with
      | ILF.Kind k -> 
	NLF.bind_fconst (Name.mk_fconst x) (compile_lf_kind outs k) outs
      | ILF.Fam a -> 
	NLF.bind_oconst (Name.mk_oconst x) (compile_lf_fam outs a) outs
      | ILF.Obj _ -> 
	Errors.not_a_kind_or_fam t
  in
  List.fold_left compile_entry (NLF.empty ()) s

let compile_lf_obj sign env obj =
  let obj = Flatten.obj (SpineForm.obj obj) in
  TypeCheck.obj sign env obj

let compile_term sign env t = 
  match SLF_LF.term sign t with
    | ILF.Obj o -> compile_lf_obj sign env o
    | _ -> assert false

let reify_term t = SLF_LF.from_obj (NLF_ILF.obj t)

let reify_kind k = SLF_LF.from_kind (NLF_ILF.kind k)

let reify_fam a = SLF_LF.from_fam (NLF_ILF.fam a)

let reify_sign s = 
  NLF.fold (fun entry acc -> match entry with
    | NLF.FDecl (c, k) -> (Name.of_fconst c, reify_kind k) :: acc
    | NLF.ODecl (c, a) -> (Name.of_oconst c, reify_fam a) :: acc
  ) s []

let init sign = 
  let sign = compile_sign sign in
  {
    sign = sign;
    env = NLF.Environment.empty ();
    head = Constants.version_o;
    name_status = Name.gen_status ()
  }

let term_from_repo t = 
  NLF.ODef (NLF.Definitions.Define (env_as_definitions t.env, 
				    t.head))

let check repo = 
 let s = reify_sign repo.sign in
  Format.printf "%a@." SLF_Pp.sign s;
  let s = compile_sign s in
  let t = reify_term (term_from_repo repo) in
  ignore (compile_term s (NLF.Environment.empty ()) t)

let commit repo t = 
  let t, a = compile_term repo.sign repo.env t in
  let extra_env, t = TypeCheck.whnf_obj repo.sign repo.env a t in
  let old_head = Name.gen_variable () in
  let commit = Name.gen_variable () in
  let commit_env = 
    repo.env 
    @@ (old_head --> repo.head) Constants.version_type 
    @@ extra_env
    @@ (commit --> t) Constants.commit_type
  in
  { repo with
    env = commit_env;
    head = Constants.version_s (NLF.HVar commit) (NLF.HVar old_head)
  }

let show repo = 
  Format.printf " signature:@.";
  SLF_Pp.sign Format.std_formatter (reify_sign repo.sign);
  Format.printf " term:@.";
  Format.printf "@[%a@]@." NLF.Pp.pp_obj (term_from_repo repo)

let checkout repo =
  let t = SLF_LF.from_obj (TypeCheck.checkout_obj repo.sign repo.env repo.head) in
  Format.printf "@[%a@]@." SLF_Pp.term t

let load () = 
  let ch = open_in_bin !Settings.repo in
  let repo = Marshal.from_channel (open_in_bin !Settings.repo) in
  Name.gen_init repo.name_status;
  close_in ch;
  repo

let save repo = 
  let ch = open_out_bin !Settings.repo in
  let repo = { repo with name_status = Name.gen_status () } in
  Marshal.to_channel ch repo [];
  close_out ch

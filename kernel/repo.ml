open Util
open NLF
open ILF

module Constants = struct
  open Name

  let commit_const = mk_fconst Settings.commit_const
  let commit_type = NLF.fconst commit_const

  let version_const = mk_fconst Settings.version_const
  let version_type = NLF.fconst version_const

  let version_o_const = mk_oconst Settings.version_o_const
  let version_o = NLF.oconst version_o_const
  let version_s c v = NLF.OApp (HConst version_o_const, [c; v])

end

type t = {
  sign : NLF.signature;
  term : NLF.obj;
}

let empty_repo = Constants.version_o

let compile_lf_fam sign ?(repo=empty_repo) fam =
  TypeCheck.fam sign Constants.commit_type repo (Flatten.fam (SpineForm.fam fam))

let compile_lf_obj sign ?(repo=empty_repo) obj =
  TypeCheck.obj sign Constants.commit_type repo (Flatten.obj (SpineForm.obj obj))

let compile_lf_kind sign ?(repo=empty_repo) kind =
  TypeCheck.kind sign Constants.commit_type repo (Flatten.kind (SpineForm.kind kind))

let compile_sign s = 
  let compile_entry outs (x, t) = 
    match SLF_LF.term outs t with
      | ILF.Kind k -> NLF.bind_fconst (Name.mk_fconst x) (compile_lf_kind outs k) outs
      | ILF.Fam a -> NLF.bind_oconst (Name.mk_oconst x) (compile_lf_fam outs a) outs
      | ILF.Obj _ -> Errors.not_a_kind_or_fam t
  in
  List.fold_left compile_entry (NLF.empty ()) s

let compile_term sign repo t = 
  match SLF_LF.term sign t with
    | ILF.Obj o -> compile_lf_obj sign ~repo o
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
    term = empty_repo
  }

let check repo = 
 let s = reify_sign repo.sign in
  Format.printf "%a@." SLF_Pp.sign s;
  let s = compile_sign s in
  let t = reify_term repo.term in
  ignore (compile_term s empty_repo t)

let commit repo term = 
  assert false

let show repo = 
  Format.printf " signature:@.";
  SLF_Pp.sign Format.std_formatter (reify_sign repo.sign);
  Format.printf " term:@.";
  Format.printf "@[%a@]@." NLF.Pp.pp_obj repo.term

let checkout repo =
  Format.printf "@[%a@]@." SLF_Pp.term (reify_term repo.term)

let load () = 
  let ch = open_in_bin !Settings.repo in
  let repo = Marshal.from_channel (open_in_bin !Settings.repo) in
  close_in ch;
  repo

let save repo = 
  let ch = open_out_bin !Settings.repo in
  Marshal.to_channel ch repo [];
  close_out ch

open Util
open NLF

type t = {
  sign : XLF.Sign.t;
  term : NLF.obj;
  varno : int
}

let rec compile_sign s : XLF.Sign.t = List.fold_left
  (fun (fsign,osign as sign) (x,t) -> match SLF_LF.term sign t with
    | LF.Kind k ->
      let k = LF_XLF.kind k in
      Util.if_debug (fun () -> Format.printf "@[%s :: %a@]@." x SLF.Pp.term (SLF_LF.from_kind (LF_XLF.from_kind k)));
      Name.Fconstmap.add (Name.mk_fconst x) k fsign, osign
    | LF.Fam a ->
      let a = LF_XLF.fam a in
      Util.if_debug (fun () -> Format.printf "@[%s :: %a@]@." x SLF.Pp.term (SLF_LF.from_fam (LF_XLF.from_fam a)));
      fsign, Name.Oconstmap.add (Name.mk_oconst x) a osign
    | LF.Obj t -> failwith ("obj in signature: "^x)
  ) XLF.Sign.empty s

let compile_term sign term =
    (fun x -> match SLF_LF.term sign x with
       | LF.Obj t -> t
       | _ -> assert false) //
      LF_XLF.obj //
      XLF_XLFf.obj //
      (fun t -> XLFf_NLF.obj sign term (t, bidon_type))

let reify_term t =
  (NLF_XLF.obj // LF_XLF.from_obj // SLF_LF.from_obj) t

let reify_sign s =
  XLF.Sign.fold (fun entry acc -> match entry with
    | XLF.FDecl (c, k) -> (Name.of_fconst c, SLF_LF.from_kind (LF_XLF.from_kind k)) :: acc
    | XLF.ODecl (c, a) -> (Name.of_oconst c, SLF_LF.from_fam (LF_XLF.from_fam a)) :: acc
  ) s []

let init sign =
  let sign = ("Bidon", Position.unknown_pos SLF.Type) :: ("bidon", Position.unknown_pos (SLF.Ident"Bidon")) :: sign in 			(* TODO temp *)
  let sign = compile_sign sign in
  {sign = sign;
   varno = Name.gen_status();
   term = bidon}				(* TODO *)

let check repo =			(* TODO temp *)
  (* LF_check.sign repo.sign; *)
  (* let t = (XLFf_NLF.from_obj // XLF_XLFf.from_obj // *)
  (* 	     LF_XLF.from_obj) repo.term in *)
  (* ignore(LF_check.obj repo.sign t) *)
  ()

let commit repo term =
  let t = compile_term repo.sign repo.term term in
  {repo with term = t; varno = Name.gen_status()}

let show repo = 
  Format.printf " signature:@.";
  SLF.Pp.sign Format.std_formatter (reify_sign repo.sign);
  Format.printf " term:@.";
  Format.printf "@[%a@]@." Pp.obj repo.term

let checkout repo =
  Format.printf "@[%a@]@." SLF.Pp.term (reify_term repo.term)

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

open Util
open NLF

type t = {
  sign : NLFSign.t;
  term : NLF.obj;
  varno : int
}

let rec compile_sign s : NLFSign.t = List.fold_left
  (fun (fsign,osign as sign) (x,t) -> match SLF_LF.term sign t with
    | LF.Kind k ->
      let k = XLF_XLFf.kind (LF_XLF.kind k) in
      let k = XLFf_NLF.kind sign bidon k in
      Util.if_debug (fun () -> Format.printf "@[%s :: %a@]@." x Pp.kind k);
      NLFSign.FDecl.add (Name.mk_fconst x) k fsign, osign
    | LF.Fam a ->
      let a = XLF_XLFf.fam (LF_XLF.fam a) in
      let a = XLFf_NLF.fam sign bidon a in
      Util.if_debug (fun () -> Format.printf "@[%s :: %a@]@." x Pp.fam a);
      fsign, NLFSign.ODecl.add (Name.mk_oconst x) a osign
    | LF.Obj t -> failwith ("obj in signature: "^x)
  ) NLFSign.empty s

let compile_term sign term =
    (fun x -> match SLF_LF.term sign x with
       | LF.Obj t -> t
       | _ -> assert false) //
      LF_XLF.obj //
      XLF_XLFf.obj //
      (fun t -> XLFf_NLF.obj sign term (t, bidon_type))

let reify_term t =
  (NLF_XLF.obj // LF_XLF.from_obj // SLF_LF.from_obj) t

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
  Pp.sign Format.std_formatter repo.sign;
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

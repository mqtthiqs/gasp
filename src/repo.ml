open Util
open NLF

type t = {
  sign : NLFSign.t;
  term : NLF.obj option
}

(* TEMP: Override to use LF_check *)
module LF_XLF = struct
let entry kont nlfs = function
  | LF.FDecl k -> 
      ignore (LF_check.kind nlfs [] LF_check.Subst.empty k);
      kont nlfs (XLF.FDecl (LF_XLF.kind k))
  | LF.ODecl a -> 
      ignore (LF_check.fam nlfs [] LF_check.Subst.empty a);
      kont nlfs (XLF.ODecl (LF_XLF.fam a))

  let obj t = LF_XLF.obj t
  let from_sign = LF_XLF.from_sign
  let from_obj = LF_XLF.from_obj
end


let compile_sign = 
  SLF_LF.sign 
    (LF_XLF.entry 
       (XLF_XLFa.entry
	  (XLFa_XLFe.entry
	     (XLFe_XLFn.entry
		(XLFn_NLF.entry
		   (fun _ x -> x)))))) NLFSign.empty

let reify_sign = XLFn_NLF.from_sign // XLFe_XLFn.from_sign // XLFa_XLFe.from_sign //
  XLF_XLFa.from_sign // LF_XLF.from_sign // SLF_LF.from_sign

let compile_term sign subst =
    (fun x -> match SLF_LF.term sign x with
       | LF.Obj t -> t	(* TEMP: LF_check *)
       | _ -> assert false) //
      LF_XLF.obj //
      XLF_XLFa.obj subst sign //
      XLFa_XLFe.obj //
      XLFe_XLFn.obj //
      XLFn_NLF.obj subst

let reify_term t =
  (XLFn_NLF.from_obj // XLFe_XLFn.from_obj // XLFa_XLFe.from_obj // XLF_XLFa.from_obj // 
     LF_XLF.from_obj // SLF_LF.from_obj) t

let init sign = 
  let sign = compile_sign sign in
  {sign = sign;
   term = None}

let check repo =			(* TODO temp *)
  ()
  (* NLF_check.sign repo.sign; *)
  (* match repo.term with *)
  (*   | None -> () *)
  (*   | Some term -> NLF_check.obj repo.sign term *)

let commit repo term =
  match repo.term with
    | None -> 
	let t = compile_term repo.sign NLFSubst.empty term in
	{repo with term = Some t}
    | Some (NLF.Obj(env,subst,h,l,c,m)) -> 
	let t = compile_term repo.sign subst term in
	{repo with term = Some t}

let show repo = 
  Format.printf " signature:@.";
  Pp.sign Format.std_formatter repo.sign;
  Format.printf " term:@.";
  match repo.term with
    | None -> Format.printf "empty.\n"
    | Some term -> Pp.obj Format.std_formatter term; Format.printf "\n"

let checkout repo =
  match repo.term with
    | None -> ()
    | Some t -> SLF.Pp.term Format.std_formatter (reify_term t)

let load () = 
  let ch = open_in_bin !Settings.repo in
  let repo = Marshal.from_channel (open_in_bin !Settings.repo) in
  close_in ch;
  repo

let save repo = 
  let ch = open_out_bin !Settings.repo in
  Marshal.to_channel ch repo [];
  close_out ch


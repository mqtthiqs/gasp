open Util
open NLF

type t = {
  sign : NLFSign.t;
  term : NLF.obj option
}

let compile_sign = SLF_LF.sign [] // LF_XLF.sign // XLF_XLFa.sign [] //
  XLFa_XLFe.sign // XLFe_NLF.sign NLFSign.empty

let reify_sign = XLFe_NLF.from_sign // XLFa_XLFe.from_sign //
  XLF_XLFa.from_sign // LF_XLF.from_sign // SLF_LF.from_sign

let compile_term (sign : NLFSign.t) (env : NLFEnv.t) t = 
  (* (fun x -> match SLF_LF.term sign env x with *)
  (*    | LF.Obj t -> t *)
  (*    | _ -> assert false) // *)
  (*   LF_XLF.obj [] // *)
  (*   XLF_XLFa.obj NLFEnv.empty sign env // *)
  (*   XLFa_XLFe.term // *)
  (*   XLFe_NLF.term NLFSign.empty *)
  assert false				(* TODO *)

let init sign = {sign = compile_sign sign;
		 term = None}

let compile repo term =
  match repo.term with
    | None -> compile_term repo.sign NLFEnv.empty term
    | Some (NLF.Obj(env,ht,ha)) -> compile_term repo.sign env term

let load repo = Marshal.from_channel (open_in_bin !Settings.repo)
let save repo = Marshal.to_channel (open_out_bin !Settings.repo) repo []


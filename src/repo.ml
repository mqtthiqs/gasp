open Util
open NLF

type t = {
  sign : NLFSign.t;
  term : NLF.obj option
}

let compile_sign = 
  SLF_LF.sign 
    (LF_XLF.entry 
       (XLF_XLFa.entry
	  (XLFa_XLFe.entry
	     (XLFe_NLF.entry
		(fun _ x -> x))))) NLFSign.empty

let reify_sign = XLFe_NLF.from_sign // XLFa_XLFe.from_sign //
  XLF_XLFa.from_sign // LF_XLF.from_sign // SLF_LF.from_sign

let compile_term (sign : NLFSign.t) (env : NLFEnv.t) t =
  assert false
    (* SLF_LF.term sign // LF_XLF.obj [] // XLF_XLFa.obj env sign (XLFa_XLFe.obj // XLFe_NLF.obj env) *)
    
    
    (* (fun x -> match SLF_LF.term sign env x with *)
    (*    | LF.Obj t -> t *)
    (*    | _ -> assert false) // *)
    (*   LF_XLF.obj [] // *)
    (*   XLF_XLFa.obj NLFEnv.empty sign env // *)
  (*   XLFa_XLFe.term // *)
    (*   XLFe_NLF.term NLFSign.empty *)

let init sign = 
  let sign = compile_sign sign in
  NLF_pp.sign Format.std_formatter sign;
  {sign = sign;
   term = None}

let compile repo term =
  match repo.term with
    | None -> compile_term repo.sign NLFEnv.empty term
    | Some (NLF.Obj(env,ht,ha)) -> compile_term repo.sign env term

let load repo = Marshal.from_channel (open_in_bin !Settings.repo)
let save repo = Marshal.to_channel (open_out_bin !Settings.repo) repo []


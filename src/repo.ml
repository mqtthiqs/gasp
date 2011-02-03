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

let compile_term (sign : NLFSign.t) (env : NLFEnv.t) =
    (* SLF_LF.term sign // LF_XLF.obj [] // XLF_XLFa.obj env sign (XLFa_XLFe.obj // XLFe_NLF.obj env) *)
    (fun x -> match SLF_LF.term sign x with
       | LF.Obj t -> t
       | _ -> assert false) //
      LF_XLF.obj [] //
      XLF_XLFa.obj env sign //
      XLFa_XLFe.obj //
      XLFe_NLF.obj env

let init sign = 
  {sign = compile_sign sign;
   term = None}

let compile repo term =
  match repo.term with
    | None -> 
	let t = compile_term repo.sign NLFEnv.empty term in
	{repo with term = Some t}
    | Some (NLF.Obj(env,ht,ha)) -> 
	let t = compile_term repo.sign env term in
	{repo with term = Some t}

let show repo = 
  Format.printf " signature:@.";
  NLF_pp.sign Format.std_formatter repo.sign;
  Format.printf " term:@.";
  match repo.term with
    | None -> Format.printf "empty.\n"
    | Some term -> NLF_pp.obj Format.std_formatter term; Format.printf "\n"
  

let load repo = Marshal.from_channel (open_in_bin !Settings.repo)
let save repo = Marshal.to_channel (open_out_bin !Settings.repo) repo []


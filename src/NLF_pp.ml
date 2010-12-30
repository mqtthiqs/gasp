open NLF
open NLF

let obj fmt t = XLF_pp.obj fmt (XLF_XLFe.from_obj (XLFe_NLF.from_obj t))
let fam fmt a = XLF_pp.fam fmt (XLF_XLFe.from_fam (XLFe_NLF.from_fam a))
let kind fmt k = XLF_pp.kind fmt (XLF_XLFe.from_kind (XLFe_NLF.from_kind k))
let env fmt e = XLF_pp.args fmt (XLF_XLFe.from_args (XLFe_NLF.from_env_args e))

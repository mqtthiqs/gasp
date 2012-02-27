open Names

module Context = struct

  module M = Map.Make(Meta)
  type t = (LF.Env.t * LF.obj * LF.fam) M.t
  let empty = M.empty
  let find = M.find
  let add = M.add
  let fold = M.fold

end

type t = {
  sign: LF.Sign.t;
  ctx: Context.t;
  head: Meta.t;
}

let empty = {sign = LF.Sign.empty; ctx = Context.empty; head = Meta.make "DUMMY"}

module Printer = struct

  let context fmt c =
    Context.fold
      (fun x (e, m, a) () ->
        let e' = LF.Env.names_of e in
        Format.fprintf fmt "%a ⊢ %a : %a = %a@."
          SLF.Printer.env e
          Meta.print x
          (SLF.Printer.efam e') a
          (SLF.Printer.eobj e') m
      ) c ()

  let t_light fmt {sign; ctx; head} =
    Format.fprintf fmt "%a ⊢ %a@."
      context ctx
      Meta.print head

  let t fmt {sign; ctx; head} =
    Format.fprintf fmt "Signature:@ %a@.Context:@ %a@ ⊢ %a@."
      SLF.Printer.sign sign
      context ctx
      Meta.print head

end

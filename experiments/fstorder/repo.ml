open Names

module Context = struct

  module M = Map.Make(Meta)
  type t = (LF.obj * LF.fam) M.t
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

module Printer = struct

  let context fmt c =
    Context.fold
      (fun x (m,a) () ->
        Format.fprintf fmt "%a : %a = %a@."
          Meta.print x
          LF.Printer.fam a
          LF.Printer.obj m
      ) c ()

  let t_light fmt {sign; ctx; head} =
    Format.fprintf fmt "%a |- %a@."
      context ctx
      Meta.print head

  let t fmt {sign; ctx; head} =
    Format.fprintf fmt "Signature:@ %a@.Context:@ %a@.|- %a@."
      LF.Printer.sign sign
      context ctx
      Meta.print head

end

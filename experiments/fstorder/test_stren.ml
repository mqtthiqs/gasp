#use "load.ml"
;;

open Util

let repo = Version.init
<:sign<
  A : type.
  f : (A -> A -> A) -> A.
  P : A -> type.
  p : {g : A -> A -> A} P (f [x] [y] g x y).
>>
;;

SLF.Printer.repo Format.std_formatter repo
;;

42

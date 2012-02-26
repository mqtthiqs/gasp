#use "load.ml"
;;

let repo = Slicer.init <:sign<
  nat : type.
  o : nat.
  s : nat -> nat.

  exp : type.
  enat : nat -> exp.
  plus : exp -> exp -> exp.

  exec : exp -> exp = $fun x -> match x with [] -> <<o>> | x :: xs -> x $.
>>

let rec nat_of_int = function
  | << o >> -> 0
  | << s $n$ >> -> nat_of_int n + 1


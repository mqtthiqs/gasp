#use "load.ml"
;;

(* Tsub: Incremental tests 1 *)

open Check_Tsub

(* #load "test_check_Tsub.cmo";; *)

Tests.commit_eq repo
<<
  infer (recb o o [x] [y] s x)
>> <<
  ex (recb o o ([x] [y] s x)) nat
  (is_rec o o ([x] [y] s x) nat (is_sub o even nat sub_even_nat is_o)
     (is_sub o even nat sub_even_nat is_o) ([x] [y] [hx] [hy] is_sn x hx))
  >>
;;

let repo = Tests.commit repo
<<
  infer (lam nat [x] s x)
>>
;;

let repo = Tests.commit repo
<<
  infer (
    lam nat [x]
      infer^0 ?j04lbjma[x] (
        ex ?j04lbjma[x] nat ?flkccrfm23[x; prj x nat (infer x)]
      )
  )
>>
;;

42

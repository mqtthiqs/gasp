#use "load.ml"
#load "test_check_T.cmo"
;;

(* Test T: batch tests *)

open Test_check_T
;;

(* Incremental tests *)

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
        ex ?j04lbjma[x] nat ?bnl0tnkq13[x; prj x nat (infer x)]
      )
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    recb (
      infer^0 ?j04lbjma[o] (
        ex ?j04lbjma[o] nat ?bnl0tnkq13[o; prj o nat (infer o)]
      )
    ) (
      infer^0 ?j04lbjma[o] (
        ex ?j04lbjma[o] nat ?bnl0tnkq13[o; prj o nat (infer o)]
      )
    )  [x] [y]
      infer^0 ?j04lbjma[x] (
        ex ?j04lbjma[x] nat ?bnl0tnkq13[x; prj x nat (infer x)]
      )
  )
>>
;;

42

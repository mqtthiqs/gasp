#use "load.ml"
;;

open Test_check_Tsub


(* Tsub: Incremental tests 2 (LFMTP paper) *)

let repo = Tests.commit repo
<<
  infer (recb (s o) (s o) [x] [y] s x)
>>
;;

Tests.commit repo
<<
  infer (recb (infer^0 ?X0 (ex ?X0 odd ?X9)) (infer^0 ?X0 (ex ?X0 odd ?X9)) [x] [y] s x)
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb (lam nat [x] lam nat [y] recb x y [z] [_] (infer^0 (s z) (ex (s z) nat ?X16[z; prj z nat (infer z)]))) [add]
    app (app add (infer^0 ?X0 (ex ?X0 odd ?X9))) (infer^0 ?X0 (ex ?X0 odd ?X9))
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb ?X42[] [add]
    letb (lam nat [x] lam nat [y] recb o y [z][_] add x z) [mul]
    app (app mul ?X43[add; infer add]) (s (infer^0 ?X23))
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb ?X42 [add]
    infer^0 (
      inline ([x] ?X90[x]) ?X91 nat ?X92[add; infer add]
    )
  )
>>
;;

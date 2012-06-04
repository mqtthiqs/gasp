#use "load.ml"
;;

open Test_check_Tsub


(* Tsub: Incremental tests 2 (LFMTP paper) *)


(* recb (s o) (s o) [x][y] s x *)
let repo = Tests.commit repo
<<
  infer (recb (s o) (s o) [x] [y] s x)
>>
;;

(* recb (s o) (s o) [x][y] s x *)
(* (redo it with sharing, with (s x) of type nat) *)
Tests.commit repo
<<
  infer (
    recb
      (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda))
      (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda))
      [x] [y] infer^0 ?k60pbjma[x] (ex ?k60pbjma[x] nat ?if35hpqm23[x; prj x nat (infer x)])
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb
      (lam nat [x] lam nat [y]
         recb x y [z] [_] (infer^0 (s z) (ex (s z) nat ?if35hpqm23[z; prj z nat (infer z)]))) [add]
      app
      (app add (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda)))
      (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda))
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb (infer^0 ?cfpx44t7vha (ex ?cfpx44t7vha (arr nat ?a8w9p8xsmga) ?bk24qt6xq5cd)) [add]
    letb (lam nat [x] lam nat [y] recb o y [z][_] app (app add x) z) [mul]
    app (app mul
           (infer^0 (app ?a87yl0low4[add] ?e34gq0ra) (ex (app ?a87yl0low4[add] ?e34gq0ra) nat
                                        ?s4cd48rnnz9fa[add; prj add ?gxcaruew4ga (infer add)])))
      (s (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda)))
  )
>>
;;

(* let repo = Tests.commit repo *)
(* << *)
(*   infer ( *)
(*     letb ?X42 [add] *)
(*     infer^0 ( *)
(*       inline ([x] ?gp2vx9rpoda[x]) ?gp2vx9rpoda nat ?gp2vx9rpoda[add; infer add] *)
(*     ) *)
(*   ) *)
(* >> *)
(* ;; *)

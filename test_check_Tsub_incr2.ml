#use "load.ml"
;;

open Test_check_Tsub


(* #load "test_check_Tsub.cmo";; *)

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
      [x] [y] infer^0 ?j04lbjma[x] (ex ?j04lbjma[x] nat ?flkccrfm23[x; prj x nat (infer x)])
  )
>>
;;

(* let add = λxy. rec x y z_. s z in
   add (s o) (s o)
   (with sharing on (s z) and (s o))
*)
let repo = Tests.commit repo
<<
  infer (
    letb
      (lam nat [x] lam nat [y]
         recb x y [z] [_] (infer^0 (s z) (ex (s z) nat ?flkccrfm23[z; prj z nat (infer z)]))) [add]
      app
      (app add (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda)))
      (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda))
  )
>>
;;

(* let add = ... in
   let mul = λxy. rec o y z_. add x z in
   mul (add (s o) (s o)) (s (s o))
   (with sharing on ... and (s o))
 *)
let repo = Tests.commit repo
<<
  infer (
    letb (
      infer^0 ?en5l7qc2sha
        (ex ?en5l7qc2sha ?gxcaruew4ga ?isn7xttxszj6a)
    ) [add]
      letb (
        lam nat [x] lam nat [y] recb o y [z][_] app (app add x) z
      ) [mul]
      app (app mul
             (infer^0
                ?mwu866oos9cda[add]
                (ex ?mwu866oos9cda[add] nat
                   ?bdtbvto0cs51[add; prj add ?gxcaruew4ga (infer add)])))
      (s (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda)))
  )
>>
;;

(* let add = ... in
   (λxy. rec o y z_. add x z) (add (s o) (s o)) (s (s o))
   (with sharing on (λxy. ...), ... and (add ...)
 *)
let repo = Tests.commit repo
<<
  infer (
    letb (
      infer^0 ?en5l7qc2sha
        (ex ?en5l7qc2sha ?gxcaruew4ga ?isn7xttxszj6a)
    ) [add]
    infer^0
      ?mqiskkfw46w1a[add; ?ipc1pwxd4cb[add]] (* let [mul] body, with mul replaced *)
      (ex
         ?mqiskkfw46w1a[add; ?ipc1pwxd4cb[add]]
         nat
         ?k1panyi4t8phb[                (* let [mul] body proof *)
           add;                            (* add *)
           prj add ?gxcaruew4ga (infer add); (* ⊢ add : N→N→N *)
           ?ipc1pwxd4cb[add]; (* mul *)
           ?vzi4010grycw[add; prj add ?gxcaruew4ga (infer add)] (* ⊢ mul : N→N→N *)
         ]
      )
  )
>>
;;

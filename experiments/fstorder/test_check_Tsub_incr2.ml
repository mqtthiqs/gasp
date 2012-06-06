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
      [x] [y] infer^0 ?k60pbjma[x] (ex ?k60pbjma[x] nat ?fe8miuauzds8[x; prj x nat (infer x)])
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb
      (lam nat [x] lam nat [y]
         recb x y [z] [_] (infer^0 (s z) (ex (s z) nat ?fe8miuauzds8[z; prj z nat (infer z)]))) [add]
      app
      (app add (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda)))
      (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda))
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb (
      infer^0 ?gy4j2pnp13[[x] ?i7xa2nijgbb[x]; nat]
        (ex ?gy4j2pnp13[[x] ?i7xa2nijgbb[x]; nat] ?gn7gsi1o13[nat; ?a8w9p8xsmga] ?wq742rd5z6esb)
    ) [add]
      letb (
        lam nat [x] lam nat [y] recb o y [z][_] app (app add x) z
      ) [mul]
      app (app mul
             (infer^0
                ?lel1wxhp13[?ih610wx79v6ha[add]; ?e34gq0ra]
                (ex ?lel1wxhp13[?ih610wx79v6ha[add]; ?e34gq0ra] nat
                   ?v4gw3zvfinhva[add; prj add ?gxcaruew4ga (infer add)])))
      (s (infer^0 ?e34gq0ra (ex ?e34gq0ra odd ?gp2vx9rpoda)))
  )
>>
;;

let repo = Tests.commit repo
<<
  infer (
    letb (
      infer^0 ?gy4j2pnp13[[x] ?i7xa2nijgbb[x]; nat]
        (ex ?gy4j2pnp13[[x] ?i7xa2nijgbb[x]; nat] ?gn7gsi1o13[nat; ?a8w9p8xsmga] ?wq742rd5z6esb)
    ) [add]
    infer^0
      ?jt1gj8164ge6[add; ?hc4zvjzt9cb[add]] (* let [mul] body, with mul replaced *)
      (ex
         ?jt1gj8164ge6[add; ?hc4zvjzt9cb[add]]
         nat
         ?bwtcf4ncwnnha[                (* let [mul] body proof *)
           add;                            (* add *)
           prj add ?gn7gsi1o13[nat; ?a8w9p8xsmga] (infer add); (* ⊢ add : N→N→N *)
           ?gy4j2pnp13[[x] ?f6nhzh6ujsaf[add; x]; nat]; (* mul *)
           ?w2ntarvx3sce[add; prj add ?gn7gsi1o13[nat; ?a8w9p8xsmga] (infer add)] (* ⊢ mul : N→N→N *)
         ]
      )
  )
>>
;;

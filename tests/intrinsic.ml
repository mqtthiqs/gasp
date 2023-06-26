#use "load.ml"
;;

let repo = Version.init
<:sign<

tp : type.
bool : tp.
nat : tp.
arr : tp -> tp -> tp.

tm : tp -> type.
tt : tm bool.
ff : tm bool.
o : tm nat.
s : tm nat -> tm nat.
lam : {A:tp} {B:tp} (tm A -> tm B) -> tm (arr A B).
app : {A:tp}{B:tp} tm (arr A B) -> tm A -> tm B.

>>
;;

Format.printf "%a@." SLF.Printer.repo repo;;

let repo = Tests.commit repo <<
app (arr nat nat) (arr nat nat) (lam (arr nat nat) (arr nat nat) [x] x) (lam nat nat [x] x)
>>

let repo = Tests.commit repo <<
app nat nat
  (app (arr nat nat) (arr nat nat)
    (lam (arr nat nat) (arr nat nat) [f] lam nat nat [x] app nat nat f x)
    (lam nat nat [x] s x))
  (s o)
>>;;

Format.printf "%a@." SLF.Printer.repo repo;;

let repo = Tests.commit repo <<
app ?lfpz6tbu0 ?lfpz6tbu0
  (app ?b4fdsj22xzttb ?b4fdsj22xzttb
    ?sf80o814d1rb
    (lam ?lfpz6tbu0 ?lfpz6tbu0 ([x] s ?cbu0uqi3k[x])))
  ?d4w7tx3rma
>>;;

Format.printf "%a@." SLF.Printer.repo repo;;

let repo = Tests.commit repo <<
app ?lfpz6tbu0 ?lfpz6tbu0
  (app ?b4fdsj22xzttb ?b4fdsj22xzttb
    ?sf80o814d1rb
    (lam ?lfpz6tbu0 ?lfpz6tbu0 ([x] s ?d4w7tx3rma)))
  ?d4w7tx3rma
>>;;

Format.printf "%a@." SLF.Printer.repo repo;;

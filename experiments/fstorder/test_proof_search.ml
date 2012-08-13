#use "load.ml"
;;

exception Failure

let repo = Version.init
<:sign<

  bool : type.
  tt : bool.
  ff : bool.

  nat : type.
  z : nat.
  s : nat -> nat.

  atom : type.
  top : atom.
  bot : atom.
  n : nat -> atom.

  eq_nat : nat -> nat -> nat = $ fun x y ->
    match* x with
      | << z >> -> (match* y with << z >> -> return << tt >>
                                | _ -> return << ff >>)
      | << s $x$ >> -> (match* y with << s $y$ >> -> return << eq_nat $x$ $y$ >>
                                    | _ -> return << ff >>)
  $.

  eq_atom : atom -> atom -> nat = $ fun x y ->
    match* x with
      | << top >> -> (match* y with << top >> -> return << tt >> | _ -> return << ff >>)
      | << bot >> -> (match* y with << bot >> -> return << tt >> | _ -> return << ff >>)
      | << n $n$ >> ->
        (match* y with << n $m$ >> -> return << eq_nat $m$ $n$ >>
        | _ -> return << ff >>)
  $.

  o : type.
  at : atom -> o.
  imp : o -> o -> o.
  conj : o -> o -> o.
  disj : o -> o -> o.

  pf : o -> type.
  top_i : pf (at top).
  bot_e : {A:o} pf (at bot) -> pf A.
  imp_i : {A:o} {B:o} (pf A -> pf B) -> pf (imp A B).
  imp_e : {A:o} {B:o} pf (imp A B) -> pf A -> pf B.
  conj_i : {A:o} {B:o} pf A -> pf B -> pf (conj A B).
  conj_e1 : {A:o} {B:o} pf (conj A B) -> pf A.
  conj_e2 : {A:o} {B:o} pf (conj A B) -> pf B.
  disj_i1 : {A:o} {B:o} pf A -> pf (disj A B).
  disj_i2 : {A:o} {B:o} pf B -> pf (disj A B).
  disj_e : {A:o} {B:o} {C:o}
    pf (disj A B) -> (pf A -> pf C) -> (pf B -> pf C) -> pf C.

  hyps : type.
  nil : hyps.
  cons : {A:o} pf A -> hyps -> hyps.

  rev_append : hyps -> hyps -> hyps = $ fun xs ys ->
    match* xs with
    | << nil >> -> return ys
    | << cons $x$ $xs$ >> -> return << rev_append $xs$ (cons $x$ $ys$) >>
  $.

  search : hyps -> {A:o} pf A = $ fun hs a ->
    Util.Debug.log "a" "search %a |- %a" SLF.Printer.term hs SLF.Printer.term a;
    match* a with
    | << conj $a$ $b$ >> -> return << conj_i $a$ $b$ (search $hs$ $a$) (search $hs$ $b$) >>
    | << disj $a$ $b$ >> ->
      (* polarity change: we can also start a select *)
      begin
        try let* x = << select $hs$ nil (disj $a$ $b$) >> in return x
        with Failure ->
          try let* x = << search $hs$ $a$ >> in return << disj_i1 $a$ $b$ $x$ >>
          with Failure -> return << disj_i2 $a$ $b$ (search $hs$ $b$) >>
      end
    | << imp $a$ $b$ >> -> return << imp_i $a$ $b$ [x] search (cons $a$ x $hs$) $b$ >>
    | << at top >> -> return << top_i >>
    | << at $p$ >> -> return << select $hs$ nil (at $p$) >>
  $.

  (* [select xs ys c] selects a hypothesis in xs, and try focus on xs@ys  *)
  select : hyps -> hyps -> {C:o} pf C = $ fun xs ys c ->
    match* xs with
    | << nil >> -> raise Failure
    | << cons $a$ $m$ $xs$ >> ->
      try let* x = << focus (rev_append $xs$ $ys$) $a$ $m$ $c$ >> in return x
      with Failure -> return << select $xs$ (cons $a$ $m$ $ys$) $c$ >>
  $.

  focus : hyps -> {A:o} pf A -> {C:o} pf C = $ fun hs a m c ->
    Util.Debug.log "a" "focus %a |- %a" SLF.Printer.term a SLF.Printer.term c;
    match* a with
    | << at bot >> -> return << bot_e $c$ $m$ >>
    | << at $q$ >> ->
      begin match* c with
      | << at $p$ >> ->
        begin match* << eq_atom $p$ $q$ >> with
        | << tt >> -> return m
        | << ff >> -> raise Failure
        end
      | _ -> raise Failure
      end
    | << imp $a$ $b$ >> ->
      return << focus $hs$ $b$ (imp_e $a$ $b$ $m$ (search $hs$ $a$)) $c$ >>
    | << disj $a$ $b$ >> ->
      return << disj_e $a$ $b$ $c$ $m$
                  ([x] search (cons $a$ x $hs$) $c$)
                  ([x] search (cons $b$ x $hs$) $c$) >>
    | << conj $a$ $b$ >> ->
      (* two choices here: *)
      (* 1. stay focused and try the two sides with backtracking: *)
      begin
        try let* x = << focus $hs$ $a$ (conj_e1 $a$ $b$ $m$) $c$ >> in return x
        with Failure ->
          let* x = << focus $hs$ $b$ (conj_e2 $a$ $b$ $m$) $c$ >> in return x
      end
      (* 2. go back to unfocused state with two new premisses: *)
      (* return << search (cons $a$ (conj_e1 $a$ $b$ $m$)
         (cons $b$ (conj_e2 $a$ $b$ $m$) $hs$)) $c$ >> *)
  $.

>>
;;

let a = << at (n z) >> ;;
let b = << at (n (s z)) >>;;

(* resulting proofs are not eta-expanded: *)
let _ = Tests.commit repo <<
  imp_i (imp (at bot) (at bot)) (imp (at bot) (at bot)) [x] x
>>
;;

(* the eta-expanded version: *)
let _ = Tests.commit repo <<
  imp_i (imp (at bot) (at bot)) (imp (at bot) (at bot)) [bb] imp_i (at bot) (at bot) [b]
    imp_e (at bot) (at bot) bb b
>>
;;

let _ = Tests.commit repo << search nil (at top) >>
;;

(* Implication & bottom *)

let _ = Tests.commit repo << search nil (imp (at bot) (at top)) >>
;;

let _ = Tests.commit repo << search nil (imp (at bot) (at bot)) >>
;;

let _ = Tests.commit repo << search nil (imp (at (bot)) (imp $a$ $b$)) >>
;;

let _ = Tests.commit repo << search nil (imp $a$ (imp (at (bot)) $b$)) >>
;;

let _ = Tests.commit repo << search nil (imp $a$ (imp (imp $a$ $b$) $b$)) >>
;;

let _ = Tests.commit repo << search nil (imp (imp $a$ $b$) (imp $a$ $b$)) >>
;;

let repo = Tests.commit repo << search nil (conj (at top) (at top)) >>
;;

(* Conjunction *)

let _ = Tests.commit repo << search nil (imp (conj $a$ $b$) $a$) >>
;;

let _ = Tests.commit repo << search nil (imp (conj $a$ $b$) $b$) >>
;;

let _ = Tests.commit repo << search nil (imp (conj $a$ $b$) (conj $b$ $a$)) >>
;;

(* Disjunction *)

let _ = Tests.commit repo << search nil (imp $a$ (disj $a$ $b$)) >>
;;

let _ = Tests.commit repo << search nil (imp $b$ (disj $a$ $b$)) >>
;;

(* let _ = Tests.commit repo << search nil (imp (disj $a$ $b$) (disj $b$ $a$)) >> *)
(* ;; *)

42


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

  eq_nat : nat -> nat -> bool = $ fun x y ->
    match* x with
      | << z >> -> (match* y with << z >> -> return << tt >>
                                | _ -> return << ff >>)
      | << s $x$ >> -> (match* y with << s $y$ >> -> return << eq_nat $x$ $y$ >>
                                    | _ -> return << ff >>)
  $.

  eq_atom : atom -> atom -> bool = $ fun x y ->
    match* x with
      | << top >> -> (match* y with << top >> -> return << tt >> | _ -> return << ff >>)
      | << bot >> -> (match* y with << bot >> -> return << tt >> | _ -> return << ff >>)
      | << n $x$ >> ->
        (match* y with << n $y$ >> -> return << eq_nat $x$ $y$ >>
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

  sorry : {A:o} pf A.

  hyps : type.
  nil : hyps.
  cons : {A:o} pf A -> hyps -> hyps.

  rev_append : hyps -> hyps -> hyps = $ fun xs ys ->
    match* xs with
    | << nil >> -> return ys
    | << cons $x$ $a$ $xs$ >> -> return << rev_append $xs$ (cons $x$ $a$ $ys$) >>
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
      (* We focus on formula [a], and remove it from the hypothesis (no contraction) *)
      try let* x = << focus (rev_append $ys$ $xs$) $a$ $m$ $c$ >> in return x
      (* If it fails, continue with xs *)
      with Failure -> return << select $xs$ (cons $a$ $m$ $ys$) $c$ >>
  $.

  assumption : hyps -> {P:atom} pf (at P) = $ fun hs p ->
    Util.Debug.log "a" "assumption %a |- %a" SLF.Printer.term hs SLF.Printer.term p;
    match* hs with
    | << nil >> -> raise Failure
    | << cons $a$ $m$ $hs$ >> -> match* a with
      | << at $q$ >> ->
        begin match* << eq_atom $p$ $q$ >> with
        | << tt >> -> return m
        | << ff >> -> return << assumption $hs$ $p$ >>
        end
      | _ -> return << assumption $hs$ $p$ >>
  $.

  focus : hyps -> {A:o} pf A -> {G:o} pf G = $ fun hs a m g ->
    Util.Debug.log "a" "focus %a |- %a" SLF.Printer.term a SLF.Printer.term g;
    match* a with
    | << at bot >> -> return << bot_e $g$ $m$ >>
    | << at $q$ >> ->
      begin match* g with
      | << at $p$ >> ->
        begin match* << eq_atom $p$ $q$ >> with
        | << tt >> -> return m
        | << ff >> -> raise Failure
        end
      | _ -> raise Failure
      end
    | << imp $a$ $b$ >> ->
      begin match* a with
      | << at $p$ >> ->
        let* n = << assumption $hs$ $p$ >> in
        return << search (cons $b$ (imp_e $a$ $b$ $m$ $n$) $hs$) $g$ >>
      | << conj $c$ $d$ >> ->
        return << search (cons (imp $c$ (imp $d$ $b$)) (imp_i $c$ (imp $d$ $b$) [pc] imp_i $d$ $b$ [pd] imp_e (conj $c$ $d$) $b$ $m$ (conj_i $c$ $d$ pc pd))
          $hs$) $g$ >>
      | << disj $c$ $d$ >> ->
        return << search
          (cons (imp $c$ $b$) (imp_i $c$ $b$ [pc] imp_e (disj $c$ $d$) $b$ $m$ (disj_i1 $c$ $d$ pc))
          (cons (imp $d$ $b$) (imp_i $d$ $b$ [pd] imp_e (disj $c$ $d$) $b$ $m$ (disj_i2 $c$ $d$ pd))
            $hs$)) $g$ >>
      | << imp $c$ $d$ >> ->
        return <<
          imp_e $b$ $g$
            (imp_i $b$ $g$ [pb] (search (cons $b$ pb $hs$) $g$))
            (imp_e (imp $c$ $d$) $b$
              $m$
              (imp_i $c$ $d$ [pc]
                (imp_e $c$ $d$
                  (imp_e (imp $d$ $b$) (imp $c$ $d$)
                    (imp_i (imp $d$ $b$) (imp $c$ $d$) [pdb]
                      (search (cons (imp $d$ $b$) pdb $hs$) (imp $c$ $d$)))
                    (imp_i $d$ $b$ [pd]
                      imp_e (imp $c$ $d$) $b$
                        $m$
                        (imp_i $c$ $d$ [pc] pd)))
                  pc)))
        >>
      end
    | << disj $a$ $b$ >> ->
      return << disj_e $a$ $b$ $g$ $m$
                  ([x] search (cons $a$ x $hs$) $g$)
                  ([x] search (cons $b$ x $hs$) $g$) >>
    | << conj $a$ $b$ >> ->
      (* two choices here: *)
      (* 1. stay focused and try the two sides with backtracking: (not complete, see ex. [1]!) *)
      (* begin *)
      (*   try let* x = << focus $hs$ $a$ (conj_e1 $a$ $b$ $m$) $c$ >> in return x *)
      (*   with Failure -> *)
      (*     let* x = << focus $hs$ $b$ (conj_e2 $a$ $b$ $m$) $c$ >> in return x *)
      (* end *)
      (* 2. go back to unfocused state with two new premisses: *)
      return << search (cons $a$ (conj_e1 $a$ $b$ $m$)
         (cons $b$ (conj_e2 $a$ $b$ $m$) $hs$)) $g$ >>
  $.

>>
;;

let a = << at (n z) >> ;;
let b = << at (n (s z)) >>;;
let c = << at (n (s (s z))) >>;;

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

let _ = List.iter (fun x -> ignore (Tests.commit repo << search nil $x$ >>))
  [
    (* Implication & bottom *)
    << at top >>;
    << imp (at bot) (at top) >>;
    << imp (at bot) (at bot) >>;
    << imp (at bot) (at bot) >>;
    << imp (at (bot)) (imp $a$ $b$) >>;
    << imp $a$ (imp (at (bot)) $b$) >>;
    << imp $a$ (imp $b$ $a$) >>;
    << imp $a$ (imp (imp $a$ $b$) $b$) >>;
    << imp (imp $a$ $b$) (imp $a$ $b$) >>;
    << imp $a$ (imp (imp $a$ $b$) (imp (imp $a$ (imp $b$ $c$)) $c$))>>;
    (* barbara *)
    << imp (imp $a$ $b$) (imp (imp $b$ $c$) (imp $a$ $c$)) >>;
    << imp (imp $c$ $a$) (imp (imp $c$ (imp $a$ $b$)) (imp $c$ $b$)) >>;
    (* S *)
    << imp (imp $a$ (imp $b$ $c$)) (imp (imp $a$ $b$) (imp $a$ $c$)) >>;

    (* Negation *)
    << imp $a$ (imp (imp $a$ (at bot)) (at bot)) >>;
    << imp (conj $a$ (imp $a$ (at bot))) (at bot) >>;     (* [1] *)
    << imp (imp (disj $a$ $b$) (at bot)) (imp $a$ $b$) >>;
    << imp (imp (disj $a$ $b$) (at bot)) (conj (imp $a$ (at bot)) (imp $b$ (at bot))) >>;
    << imp (imp $a$ $b$) (imp (imp $b$ (at bot)) (imp $a$ (at bot))) >>;
    << imp (imp $a$ $b$) (imp (imp $b$ (at bot)) (imp $a$ (at bot))) >>;
    << imp (imp $a$ (at bot)) (imp (imp (imp $a$ (at bot)) (at bot)) (at bot)) >>;
    << imp (imp (imp (imp $a$ (at bot)) (at bot)) (at bot)) (imp $a$ (at bot)) >>;

    (* Conjunction *)
    << conj (at top) (at top) >>;
    << imp $a$ (imp $b$ (conj $a$ $b$)) >>;
    << imp (conj $a$ $b$) $a$ >>;
    << imp (conj $a$ $b$) $b$ >>;
    << imp (conj $a$ $b$) (conj $b$ $a$) >>;

    (* Disjunction *)
    << imp $a$ (disj $a$ $b$) >>;
    << imp $b$ (disj $a$ $b$) >>;
    << imp (disj $a$ $b$) (disj $b$ $a$) >>;
    << imp (conj $a$ $b$) (disj $b$ $a$) >>;
    (* example showing that we must contract a imp_l rule: *)
    << imp (imp (disj $a$ (imp $a$ (at bot))) (at bot)) (at bot) >>
  ]
;;

(* let _ = *)
(*   Util.Debug.log "a" "repo: %a" SLF.Printer.term (Version.checkout repo) *)
(* ;; *)

42


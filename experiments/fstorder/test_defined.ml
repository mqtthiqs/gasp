#use "load.ml"
;;

let test_commit repo m n =
  let repo = Slicer.commit repo Struct.Env.empty m in
  let p = SLF.Strat.obj repo.Struct.Repo.sign [] (Slicer.checkout repo) in
  let n = SLF.Strat.obj repo.Struct.Repo.sign [] n in
  Kernel.Conv.obj repo (n, p);
  repo
;;

let reduce repo env m =
  let env = SLF.Strat.env repo.Struct.Repo.sign env in
  let repo = Slicer.commit repo env m in
  Slicer.checkout repo

let repo = Slicer.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  two : nat = s (s o).

  plus : {m:nat} {n:nat} nat = $ fun m n -> match m with
    | << o >> -> n
    | << s $m$ >> -> << s (plus $m$ $n$) >>
    | << $m$ >> -> assert false
  $.

  exp : type.
  enat : nat -> exp.
  eplus : exp -> exp -> exp.

  eval : {e : exp} nat = $ fun e -> match e with
    | << enat $n$ >> -> n
    | << eplus $e1$ $e2$ >> -> << plus (eval $e1$) (eval $e2$) >>
    (* | << ? $x$[$s$] >> -> *)
    (*   let e, m, a = Struct.Context.find (Names.Meta.make x) repo.Struct.Repo.ctx in *)
    (*   let s = List.map (SLF.Strat.obj repo.Struct.Repo.sign []) s in *)
    (*   let s = List.map (fun _ -> LF.inj @@ LF.OApp(LF.HConst (Names.OConst.make "o"), [])) s in *)
    (*   let m = LF.Subst.obj s m in *)
    (*   let m = SLF.Unstrat.obj [] m in *)
    (*   Format.printf "**** le repo:@.%a@." SLF.Printer.repo repo; *)
    (*   << eval $m$ >> *)
    | << $a$ >> -> SLF.Printer.term Format.std_formatter a; assert false
  $.
>>
;;

test_commit repo <<
  eval (enat o)
>> <<
  o
>>
;;

test_commit repo <<
  eval (eplus (enat o) (enat o))
>> <<
  o
>>
;;

(* test_commit repo << *)
(*   eval (eplus (enat two) (enat two)) *)
(* >> << *)
(*   s (s (s (s o))) *)
(* >> *)
(* ;; *)

42

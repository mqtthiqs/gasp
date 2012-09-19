#use "load.ml"
;;

let repo = Version.init
<:sign<

i : type.
o : type.

O : i.
S : i -> i.
plus : i -> i -> i.
times : i -> i -> i.

eq : i -> i -> o.
lt : i -> i -> o.

not : o -> o.
conj : o -> o -> o.
disj : o -> o -> o.
imp : o -> o -> o.
forall : (i -> o) -> o.
exist : (i -> o) -> o.

tt : o -> type.

raa : {p : o} tt(not(not p)) -> tt p.
imp_i : {p : o} {q : o} (tt p -> tt q) -> tt (imp p q).
all_e : {F : i -> o} {x : i} tt(forall [x] F x) -> tt(F x).
all_i : {F: i -> o} ({x : i} tt (F x)) -> tt(forall [x] F x).

some_e : {F : i -> o} {p : o} tt(exist [x] F x) -> ({x:i} tt (F x) -> tt p) -> tt p.

sub : {t : i} {u : i} {F : i -> o} tt(eq t u) -> tt(F t) -> tt(F u).
ind : {F : i -> o} tt (F O) -> ({x:i} tt(F x) -> tt(F (S x))) -> ({x:i} tt (F x)).

commit : {A : o} tt A -> Commit.

>>
;;

let repo = Tests.commit repo <<
  commit
  (forall [x] imp (eq O x) (eq O x))
  (all_i                           (* true (forall [x] imp (eq O x) (eq O x)) *)
     ([x] (imp (eq O x) (eq O x)))                  (* i -> o *)
   [x]
       imp_i                       (* true (imp (eq O x) (eq O x)) *)
       (eq O x)                   (* o *)
       (eq O x)                   (* o *)
       ([H] H)                    (* true (eq O x) *)
  )
>>
;;

include types of mli with 
  module Idmap = Map.Make(struct type t = ident let compare=Pervasives.compare end)

module P = Position

let rec equals_term subst t u = 
  match Position.value t, Position.value u with
    | Type, Type -> true
    | Prod(x,a,b), Prod(x',a',b') ->
	equals_term subst a a' && equals_term (Idmap.add x x' subst)b b'
    | Prod(x,a,b), Arr(a',b') 
    | Arr(a,b), Prod (x,a',b') -> 
	equals_term subst a a' && equals_term subst b b'
    | Arr(t,u), Arr(t',u') -> equals_term subst t t' && equals_term subst u u'
    | Lam(x,a,b), Lam(x',a',b') ->
	x=x' && equals_term subst a a' && equals_term (Idmap.add x x' subst) b b'
    | App(t,u), App(t',u') -> equals_term subst t t' && equals_term subst u u'
    | Var x, Var x' -> 
	(try x = Idmap.find x' subst
	with Not_found -> false)
    | _, Lam(x,_,{P.value=App(u, {P.value=Var x'})}) (* Eta *)
    | Lam(x,_,{P.value=App(u, {P.value=Var x'})}), _ when x=x' ->
	equals_term subst t u
    | _ -> false

let rec equals_sign subst s s' = 
  match s, s' with
    | [], [] -> true
    | (x, Decl t) :: s, (y, Decl u) :: s' ->
	equals_term subst t u && equals_sign (Idmap.add x y subst) s s'
    | _ -> false

open Names

type fam =
  | FApp of FConst.t * obj list
  | FProd of string option * fam * fam

and obj =
  | OApp of OConst.t * obj list
  | OVar of int
  | OMeta of Meta.t

type kind =
  | KType
  | KProd of string option * fam * kind

type env = fam list

module Env = struct
  type t = fam list
  let empty = []
  let find x l = List.nth l x
  let add a l = (a :: l)
end

module Sign = struct

  type entry =
    | OConst of fam
    | FConst of kind

  module MO = Map.Make(OConst)
  module MF = Map.Make(FConst)

  type t = fam MO.t * kind MF.t
  let empty = MO.empty, MF.empty
  let ofind x ((o, f):t) = MO.find x o
  let ffind x ((o, f):t) = MF.find x f
  let oadd x a ((o, f):t) = MO.add x a o, f
  let fadd x k ((o, f):t) = o, MF.add x k f
end

module Subst = struct

  let rec lift = function
    | OApp (c, l) -> OApp (c, List.map lift l)
    | OVar x -> OVar (x+1)
    | m -> m

  let rec obj m = function
    | OApp (c, l) -> OApp (c, List.map (obj m) l)
    | m -> m

  let rec fam m = function
    | FApp (c, l) -> FApp (c, List.map (obj m) l)
    | FProd (x, a, b) -> FProd (x, fam m a, fam (lift m) b)

  let rec kind m = function
    | KType -> KType
    | KProd (x, a, k) -> KProd (x, fam m a, kind (lift m) k)
end

module Strat = struct

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj

  open SLF
  open Util

  let rec app sign env l = function
    | Ident x -> OApp (OConst.make x, l)
    | App (t, u) ->
      begin match term sign env u with
        | Obj u ->  app sign env (u :: l) t
        | _ -> failwith "stratification error"
      end
    | _ -> failwith "app error"

  and term sign env = function
    | Type -> Kind(KType)
    | Prod(x, a, b) ->
      begin match term sign env a, term sign (x::env) b with
        | Fam a, Kind k -> Kind (KProd (x, a, k))
        | Fam a, Fam b -> Fam (FProd (x, a, b))
        | _ -> failwith "stratification error"
      end
    | App (t, u) as a -> Obj (app sign env [] a)
    | Ident x ->
      begin try Obj (OVar (List.index (Some x) env))
        with Not_found ->
          try let x = OConst.make x in
              ignore(Sign.ofind x sign); Obj (OApp (x, []))
          with Not_found ->
            let x = FConst.make x in
            ignore(Sign.ffind x sign); Fam (FApp (x, []))
      end
    | Meta x -> Obj (OMeta (Meta.make x))

  let rec sign s = function
    | Nil -> s
    | Cons (c, t, s') -> match term s [] t with
        | Obj _ -> failwith "object in sign"
        | Fam a -> sign (Sign.oadd (Names.OConst.make c) a s) s'
        | Kind k -> sign (Sign.fadd (Names.FConst.make c) k s) s'
end

module Unstrat = struct

  open SLF

  let rec obj env = function
    | OApp (f, l) -> List.fold_left (fun t m -> App (t, obj env m)) (Ident (Names.OConst.repr f)) l
    | OMeta x -> Meta (Names.Meta.repr x)
    | OVar x -> match List.nth env x with
        | Some x -> Ident x
        | None -> failwith "none"

  let rec fam env = function
    | FApp (f, l) -> List.fold_left (fun t m -> App (t, obj env m)) (Ident (Names.FConst.repr f)) l
    | FProd (x, a, b) -> Prod (x, fam env a, fam (x :: env) b)

  let rec kind env = function
    | KType -> Type
    | KProd (x, a, b) -> Prod (x, fam env a, kind (x :: env) b)

end

module Util = struct

  let rec fold_meta f = function
    | OApp (c, l) -> OApp (c, List.map (fold_meta f) l)
    | OVar x -> OVar x
    | OMeta x -> f x
end

module Printer = struct
  let obj fmt m = SLF.Printer.term fmt (Unstrat.obj [] m)
  let fam fmt a = SLF.Printer.term fmt (Unstrat.fam [] a)
  let kind fmt k = SLF.Printer.term fmt (Unstrat.kind [] k)
  let entity fmt = function
    | Strat.Kind k -> kind fmt k
    | Strat.Fam a -> fam fmt a
    | Strat.Obj m -> obj fmt m

  let sign fmt (s : Sign.t) =
    let l =
      Sign.MO.fold
      (fun x a l -> SLF.Cons (Names.OConst.repr x, Unstrat.fam [] a, l)) (fst s)
      (Sign.MF.fold
         (fun x k l -> SLF.Cons (Names.FConst.repr x, Unstrat.kind [] k, l)) (snd s)
         SLF.Nil) in
    SLF.Printer.sign fmt l
end


(* module Quot = struct *)

(*   open Camlp4.PreCast.Syntax.Quotation *)

(*   let expand_term_quot loc _ s = Gram.parse_string term_eoi loc s *)
(*   let expand_sign_quot loc _ s = Gram.parse_string sign_eoi loc s *)

(*   let _ = *)
(*     add "term" DynAst.expr_tag expand_term_quot; *)
(*     add "sign" DynAst.expr_tag expand_sign_quot; *)
(* end *)

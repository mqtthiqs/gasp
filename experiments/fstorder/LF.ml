open Names

type fam =
  | FApp of FConst.t * obj list
  | FProd of string option * fam * fam

and obj =
  | OLam of string * obj
  | OApp of head * spine
  | OMeta of Meta.t

and spine = obj list

and head =
  | HVar of int
  | HConst of OConst.t

type kind =
  | KType
  | KProd of string option * fam * kind

type env = fam list

module Env = struct
  type t = fam list
  let empty = []
  let find x l = List.nth l x
  let add a l = (a :: l)
  let to_list l = l
end

module Sign = struct

  module MO = Map.Make(OConst)
  module MF = Map.Make(FConst)

  type t = (bool * fam) MO.t * kind MF.t
  let empty = MO.empty, MF.empty
  let slices x (o, f) = fst (MO.find x o)
  let ofind x ((o, f):t) = snd (MO.find x o)
  let ffind x ((o, f):t) = MF.find x f
  let oadd x a ((o, f):t) = MO.add x a o, f
  let fadd x k ((o, f):t) = o, MF.add x k f
end

module Subst = struct

  let rec lift k n = function
    | OLam (x, m) -> OLam (x, lift (k+1) n m)
    | OApp (h, l) -> OApp (head k n h, List.map (lift k n) l)
    | OMeta _ as x -> x

  and head k n = function
    | HVar x -> if x < k then HVar x else HVar (x+n)
    | HConst _ as c -> c

  let rec obj m = function
    | OApp (c, l) -> OApp (c, List.map (obj m) l)
    | m -> m

  let rec fam m = function
    | FApp (c, l) -> FApp (c, List.map (obj m) l)
    | FProd (x, a, b) -> FProd (x, fam m a, fam (lift 0 1 m) b)

  let rec kind m = function
    | KType -> KType
    | KProd (x, a, k) -> KProd (x, fam m a, kind (lift 0 1 m) k)
end

module Strat = struct

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj

  open SLF
  open Util

  let rec app sign env l = function
    | Ident x ->
      begin
        try let i = List.index (Some x) env in
            Obj (OApp (HVar i, l))
        with Not_found ->
          try let x = OConst.make x in
              ignore (Sign.ofind x sign);
              Obj (OApp (HConst x, l))
          with Not_found ->
            let x = FConst.make x in
            try ignore(Sign.ffind x sign); Fam (FApp (x, l))
            with Not_found -> failwith ("strat: not found "^FConst.repr x)
      end
    | App (t, u) ->
      begin match term sign env u with
        | Obj u ->  app sign env (u :: l) t
        | _ -> failwith "strat: argument is not an object"
      end
    | _ -> failwith "strat: app error"

  and term sign env = function
    | Lam (x, t) ->
      begin match term sign (Some x :: env) t with
        | Obj m -> Obj (OLam (x, m))
        | _ -> failwith "strat: lambda in not-an-obj"
      end
    | Type -> Kind(KType)
    | Prod (x, a, b) ->
      begin match term sign env a, term sign (x::env) b with
        | Fam a, Kind k -> Kind (KProd (x, a, k))
        | Fam a, Fam b -> Fam (FProd (x, a, b))
        | Kind _, _ -> failwith "strat: prod argument is a kind"
        | Fam _, Obj _ -> failwith "strat: prod body is an obj"
        | Obj _, _ -> failwith "strat: prod argument is an obj"
      end
    | App (t, u) as a ->
      app sign env [] a
    | Ident x ->
      begin try Obj (OApp (HVar (List.index (Some x) env), []))
        with Not_found ->
          try let x = OConst.make x in
              ignore(Sign.ofind x sign); Obj (OApp (HConst x, []))
          with Not_found ->
            let x = FConst.make x in
            try ignore(Sign.ffind x sign); Fam (FApp (x, []))
            with Not_found -> failwith ("strat: not found "^FConst.repr x)
      end
    | Meta x -> Obj (OMeta (Meta.make x))

  let obj sign env t = match term sign env t with
    | Obj m -> m
    | _ -> failwith "strat: not an obj"

  let fam sign env t = match term sign env t with
    | Fam a -> a
    | _ -> failwith "strat: not a fam"

  let kind sign env t = match term sign env t with
    | Kind k -> k
    | _ -> failwith "strat: not a kind"
end

module Unstrat = struct

  open SLF

  let rec obj env = function
    | OLam (x, m) -> Lam (x, obj (Some x :: env) m)
    | OApp (h, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (head env h)) l
    | OMeta x -> Meta (Names.Meta.repr x)

  and head env = function
    | HConst c -> Names.OConst.repr c
    | HVar x ->
      try match Env.find x env with
        | Some x -> x
        | None -> failwith "none"
      with _ -> "_UNBOUND_REL_"^(string_of_int x)

  let rec fam env = function
    | FApp (f, l) -> List.fold_left
      (fun t m -> App (t, obj env m)
      ) (Ident (Names.FConst.repr f)) l
    | FProd (x, a, b) -> Prod (x, fam env a, fam (x :: env) b)

  let rec kind env = function
    | KType -> Type
    | KProd (x, a, b) -> Prod (x, fam env a, kind (x :: env) b)

end

module Util = struct

  let rec fold_meta f = function
    | OApp (h, l) -> OApp (h, List.map (fold_meta f) l)
    | OLam (x, m) -> OLam (x, fold_meta f m)
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
      (fun x (b, a) l -> (Names.OConst.repr x, Unstrat.fam [] a, b) :: l) (fst s)
      (Sign.MF.fold
         (fun x k l -> (Names.FConst.repr x, Unstrat.kind [] k, true) :: l) (snd s)
         []) in
    SLF.Printer.sign fmt l
end


module Parser = struct

  open Camlp4.PreCast
  open Camlp4.PreCast

  let expand_obj_quot loc a s : (* LF.obj *) Ast.expr =
    SLF.Parser.expand_term_quot loc a s

  let expand_sign_quot loc a s : (* LF.sign *) Ast.expr =
    SLF.Parser.expand_sign_quot loc a s

  let _ =
    Syntax.Quotation.add "obj" Syntax.Quotation.DynAst.expr_tag expand_obj_quot;
    Syntax.Quotation.add "sign" Syntax.Quotation.DynAst.expr_tag expand_sign_quot;

end

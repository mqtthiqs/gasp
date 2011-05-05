open Name

module type SET = sig
  type t
  type key
  type value
  val add : key -> value -> t -> t
  val find : key -> t -> value
  val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
  val is_empty : t -> bool
  val empty : t
end

module rec ALF : sig
  type subst = ALFSubst.t

  type kind =
    | KType
    | KProd of variable * fam * kind

  and fam =
    | FProd of variable * fam * fam
    | FHead of subst * constant * args

  and ohead = XLFn.ohead

  and obj =
    | OLam of variable * fam * obj
    | OHead of subst * ohead * args * constant * args
    | OMeta of subst * definition * constant * args

  and arg =
    | ALam of variable * fam * obj
    | AHead of ohead * constant * args
    | AMeta of definition * constant * args

  and args = (variable * arg) list

  type entry =
    | FDecl of kind
    | ODecl of fam
  type sign = ALFSign.t
end

and ALFSubst : (SET with type key = definition
		    and type value = ALF.ohead * ALF.args * constant * ALF.args)
and ALFSign : (SET with type key = constant and type value = ALF.entry)
(* and ALFArgs : (SET with type key = variable and type value = ALF.arg) *)

module Pp : sig
  open ALF
  open Format
  val sign : formatter -> ALF.sign -> unit
  val obj : formatter -> obj -> unit
  val fam : formatter -> fam -> unit
  val kind : formatter -> kind -> unit
  val entry : formatter -> entry -> unit
end

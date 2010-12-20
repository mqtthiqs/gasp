open Name
open NLF

module CMap : sig 
  type 'a t
  val add : variable -> 'a -> 'a t -> 'a t
  val find : variable -> 'a t -> 'a
  val empty : 'a t
end

type names = variable list
type arity = names CMap.t

val obj : arity -> NLFEnv.t -> names -> XLFe.obj -> NLF.obj * names
val fam : arity -> NLFEnv.t -> names -> XLFe.fam -> NLF.fam * names
val kind : arity -> NLFEnv.t -> names -> XLFe.kind -> NLF.kind * names
val sign : arity -> NLFSign.t -> XLFe.sign -> NLFSign.t

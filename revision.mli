open Core.Std

module type Isolatable = sig
  type t
  val merge: t -> t -> t -> t
end


module type Revision = sig
  type t
  type isolated
  type value

  val create:  t -> value -> (t*isolated)
  val fork: t -> (t -> t) -> t
  val join: t -> t -> t
  val init: unit -> t
  val write: t -> isolated -> value -> t
  val read: t -> isolated -> value option

end

module Make(X:Isolatable) : (Revision with type value = X.t and type isolated = int * X.t)


  

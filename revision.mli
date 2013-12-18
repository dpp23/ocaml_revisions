open Core.Std

module type Isolatable = sig
  type t
  val merge: t -> t -> t -> t
end

module type Isolated = sig
  type t 
  type value
  val merge: t -> t -> t -> t
  val create: value -> int -> t * int
  val get_id: t -> int
  val update: t -> value -> t
  val read: t -> value
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

module Revise(X:Isolated) : (Revision with type value = X.value and type isolated = X.t)

module Isolate(X:Isolatable) :(Isolated with type t = (int*X.t) and type value = X.t)
  

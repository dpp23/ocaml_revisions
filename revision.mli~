open Core.Std

module type Isolatable = sig
  type t
  val merge: t -> t -> t -> t
end

module type Isolated = sig
  type t 
  type var
  val merge: t -> t -> t -> t
  val create: var -> t
  val get_id: t -> int
  val update: t -> var -> t
  val read: t -> var
end

module type Revision = sig
  type t
  type isolated
  type var

  val create:  t -> var -> (t*isolated)
  val fork: t -> (t -> t) -> t
  val join: t -> t -> t
  val init: unit -> t
  val write: t -> isolated -> var -> t
  val read: t -> isolated -> var option

end

module Revise(X:Isolated) : (Revision with type var = X.var and type isolated = X.t)

module Isolate(X:Isolatable) :(Isolated with type t = (int*X.t) and type var = X.t)
  

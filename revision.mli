open Core.Std
open Async.Std

exception Isolated_Not_Found
exception Incompatible_Join

module type Isolatable = sig
  type t
  val merge: t -> t -> t -> t
end


module type Revision = sig
  type i
  type result
  type t
  type isolated
  type value

  val get_revision: result -> t
  val get_isolated: result -> isolated
  val create:  t -> value -> result
  val fork: t -> (t -> t Deferred.t) -> t Deferred.t
  val join: t -> t -> t
  val init: unit -> t
  val write: t -> isolated -> value -> t
  val read: t -> isolated -> value Deferred.t
  val determine_revision: t -> t

end

module Make(X:Isolatable) : (Revision with type value = X.t and type isolated = (int * X.t) Deferred.t)

  

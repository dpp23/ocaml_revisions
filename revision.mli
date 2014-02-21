open Core.Std
open Async.Std

exception Isolated_Not_Found
exception Incompatible_Join

module type Isolatable = sig
  (** Type to be isolated **)
  type t
  (** Merge function: merge [head] [parent] [current] **)
  val merge: t -> t -> t -> t
end


module type Revision = sig
  type i
  type result
  type t
  type isolated
  type value

  val init: unit -> t
  (** Adds a new isolated with [value] and returns a new result **)
  val create:  t -> value -> result
  
  (** For breaking the result into revision and isolated **)
  val get_revision: result -> t
  val get_isolated: result -> isolated
  
  (** Scheduling primitives **)
  val fork: t -> (t -> t Deferred.t) -> t Deferred.t
  val join: t -> t -> t
  
  (** Isolated access **)
  val write: t -> isolated -> value -> t
  val read: t -> isolated -> value Deferred.t
  val determine_revision: t -> t

end

module Make(X:Isolatable) : (Revision with type value = X.t and type isolated = (int * X.t) Deferred.t)

  

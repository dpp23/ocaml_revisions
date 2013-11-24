module type Isolated = sig
  type t

  val merge: t -> t -> t -> t
  val create: unit -> t
end

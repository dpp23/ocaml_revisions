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

module Isolate(X:Isolatable) 
      :(Isolated with type t = (int*X.t) and type var = X.t)  = struct
  type t = (int*X.t)
  type var = X.t
  let id = ref 0
  let merge a b c = let (id, a) = a and (_, b) = b and (_, c) = c in
                      let n = X.merge a b c in
                        (id, n) (**TODO: check IDs match or raise exception**)   
  let create init = let old_id = !id in
                      id := !id + 1;
                      (old_id,init)
  let get_id (id,_) = id
  let update (id, _) new_v = (id, new_v)
  let read (_, v) = v
  
end

module Revise(X:Isolated) : (Revision with type var = X.var and type isolated = X.t) = struct
  (** Contains a map and a list of writen Isolated **)
  type t = ((int, X.t, Int.comparator) Map.t) * int list 
  type var = X.var
  type isolated = X.t

  let create (parent, l) init = let isolated = X.create init in
                                               (((Map.add parent ~key:(X.get_id isolated) ~data:isolated), l) , isolated) 

  let fork a f = f a
  let rec join (a, al) (b, bl) = match bl with
                                [] -> (a, al)
                               |x::xs -> let k = Map.find b x in
                                             match k with
                                              Some(y) -> join (Map.add a ~key:x ~data:y,(x::al)) (b, xs)
                                              | None -> assert(false); join (a, al) (b,xs)
  let write (t,l) iso v = (Map.add t ~key:(X.get_id iso) ~data:(X.update iso v), (X.get_id iso)::l)
  let read (a, _) iso = match Map.find a (X.get_id iso) with
                              Some v -> Some (X.read v)
                             |None -> None
  let init () = Map.empty ~comparator:Int.comparator, []

end

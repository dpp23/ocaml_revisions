open Core.Std

module type Isolatable = sig
  type t
  val merge: t -> t -> t -> t
end

module type Isolated = sig
  type t 
  type value
  val merge: t -> t -> t -> t
  val create: value -> t
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

module Isolate(X:Isolatable) 
      :(Isolated with type t = (int*X.t) and type value = X.t)  = struct
  type t = (int*X.t)
  type value = X.t
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

module Revise(X:Isolated) : (Revision with type value = X.value and type isolated = X.t) = struct
  (** Contains a map that represents the state of the revision and one that is the state of the mother revision and a list of writen Isolated **)
  type t = ((int, X.t, Int.comparator) Map.t) * ((int, X.t, Int.comparator) Map.t) * int list 
  type value = X.value
  type isolated = X.t

  let create (parent, _, l) init = let isolated = X.create init in 
                                                    let k = (Map.add parent ~key:(X.get_id isolated) ~data:isolated) in
                                                      ((k, k, l), isolated) 

  let fork a f = f a

  let rec join (a, ap, al) (b, bp, bl) = match bl with
                                [] -> (a, ap, al)
                               |x::xs -> let k = Map.find b x in
                                           let kp = Map.find bp x in
                                             let ka = Map.find a x in
                                               match k with
                                                 Some(y) -> begin match kp with
                                                              Some(yp)-> begin match ka with
                                                                         Some(ya)-> join (Map.add a ~key:x ~data:(X.merge yp ya y), ap, (x::al)) (b, bp, xs)
                                                                         |None -> join (a, ap, al) (b, bp, xs)
                                                                       end
                                                              |None -> join (a, ap, al) (b, bp, xs)
                                                            end
                                                 | None -> join (a, ap, al) (b, bp, xs)

  let write (t,p,l) iso v = (Map.add t ~key:(X.get_id iso) ~data:(X.update iso v), p, (X.get_id iso)::l)
  let read (a,_, _) iso = match Map.find a (X.get_id iso) with
                              Some v -> Some (X.read v)
                             |None -> None
  let init () = Map.empty ~comparator:Int.comparator, Map.empty ~comparator:Int.comparator, []

end

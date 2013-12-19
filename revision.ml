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

module Make(X:Isolatable) : (Revision with type value = X.t and type isolated = int * X.t) = struct
  module Isolated = struct 
    type t = (int*X.t)
    type value = X.t
    let merge a b c = let (id, a) = a and (_, b) = b and (_, c) = c in
                        let n = X.merge a b c in
                          (id, n) (**TODO: check IDs match or raise exception**)   
    let create init seq = ((seq, init), seq + 1)
    let get_id (id,_) = id
    let update (id, _) new_v = (id, new_v)
    let read (_, v) = v

  end
  
  module WrittenSet = Set.Make(Int)
  (** Contains a map that represents the state of the revision and one that is the state of the mother revision and a list of writen Isolated **)
  type t = ((int, Isolated.t, Int.comparator) Map.t) * ((int, Isolated.t, Int.comparator) Map.t) * WrittenSet.t * int 
  type value = X.t
  type isolated = Isolated.t

  let create (parent, _, l, s) init = let (isolated, seq) = Isolated.create init s in 
                                                    let k = (Map.add parent ~key:(Isolated.get_id isolated) ~data:isolated) in
                                                      ((k, k, l, seq), isolated) 

  let fork a f = f a
  
  let join (a, ap, al, aseq) (b, bp, bl, bseq) = let wlist = WrittenSet.elements bl in
                                                    let rec join_rec (a, ap, al, aseq) (b, bp, bl, bseq) = match bl with
                                                      [] -> (a, ap, al, aseq)
                                                      |x::xs -> let k = Map.find b x and kp = Map.find bp x and ka = Map.find a x in
                                                                   match (k, kp, ka) with
                                                                     (Some(y), Some(yp), Some(ya)) -> join_rec 
                                                                      (Map.add a ~key:x ~data:(Isolated.merge yp ya y), ap, WrittenSet.add al x, aseq) (b, bp, xs, bseq)
                                                                     |_ -> join_rec (a, ap, al, aseq) (b, bp, xs, bseq)
                                                    in
                                                      join_rec (a, ap, al, aseq) (b, bp, wlist, bseq)
            
  

  let write (t,p,l,s) iso v = (Map.add t ~key:(Isolated.get_id iso) ~data:(Isolated.update iso v), p, WrittenSet.add l (Isolated.get_id iso), s)
  let read (a,_, _, _) iso = match Map.find a (Isolated.get_id iso) with
                              Some v -> Some (Isolated.read v)
                             |None -> None
  let init () = Map.empty ~comparator:Int.comparator, Map.empty ~comparator:Int.comparator, WrittenSet.empty, 0

end

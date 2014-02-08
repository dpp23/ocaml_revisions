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
  type t
  type isolated
  type value
  type result

  val get_revision: result -> t
  val get_isolated: result -> isolated 
  val create:  t -> value -> result
  val fork: t -> (t -> t Deferred.t) -> t Deferred.t
  val join: t -> t -> t
  val init: unit -> t
  val write: t -> isolated -> value -> t
  val read: t -> isolated -> value Deferred.t

end

module Make(X:Isolatable) : (Revision with type value = X.t and type isolated = (int * X.t) Deferred.t) = struct
  module Isolated = struct 
    type t = int*X.t
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
  type i = 
        { parent : ((int, Isolated.t, Int.comparator) Map.t);
          self : ((int, Isolated.t, Int.comparator) Map.t);
          written : WrittenSet.t;
          id : int ;
          fork_exn : exn option 
        }
  type t = i Deferred.t  
  type value = X.t
  type isolated = Isolated.t Deferred.t
  type result = (i * Isolated.t) Deferred.t 

  let create parent init =  parent >>| fun parent -> let (isolated, seq) = Isolated.create init parent.id in 
                                                    let k = (Map.add parent.parent ~key:(Isolated.get_id isolated) ~data:isolated) in
                                                      ({parent =  k; self = k;  written = parent.written; id = seq; fork_exn = None}, isolated) 

  let fork a f = try f a with e -> a >>| fun a -> return {a with fork_exn = Some(e)}
  
  let join a b = Deferred.both a b 
                 >>| fun (a,b) -> 
                       match a.fork_exn with
                        |Some(e) -> raise e
                        |None -> let wlist = WrittenSet.elements b.written in
                                                    let rec join_rec a b wrt = match wrt with
                                                      [] -> a
                                                      |x::xs -> let k = Map.find b.self x and kp = Map.find b.parent x and ka = Map.find a.self x in
                                                                   match (k, kp, ka) with
                                                                     (Some(y), Some(yp), Some(ya)) -> join_rec 
                                                                      { a with self = Map.add a.self ~key:x ~data:(Isolated.merge yp ya y);
                                                                               written = WrittenSet.add a.written x
                                                                      } b xs
                                                                     |_ -> raise Incompatible_Join
                                                    in
                                                      join_rec a b wlist 
            
  

  let write a iso v = Deferred.both a iso 
                      >>| fun (a, iso) ->  
                           match Map.find a.self (Isolated.get_id iso) with
                             Some _ -> { a with self = Map.add a.self ~key:(Isolated.get_id iso) ~data:(Isolated.update iso v);
                                          written = WrittenSet.add a.written (Isolated.get_id iso)
                                       }
                             |None -> raise Isolated_Not_Found
  let read a iso = Deferred.both a iso >>| fun (a, iso) -> match Map.find a.self (Isolated.get_id iso) with
                                    Some v -> Isolated.read v
                                   |None -> raise Isolated_Not_Found
  let init () = return
                { self = Map.empty ~comparator:Int.comparator;
                  parent = Map.empty ~comparator:Int.comparator;
                  written = WrittenSet.empty;
                  id = 0;
                  fork_exn = None
                }
  let get_revision (res : result) = res >>| fun (a, _) -> a
  let get_isolated (res : result) = res >>| fun (_, b) -> b

end

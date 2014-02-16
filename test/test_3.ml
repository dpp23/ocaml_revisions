open Core.Std
open Async.Std
open Revision

module IntRevision = Make(struct 
    type t = int
    let merge a b c = c - b + a
  end)

let rec test rev iso c = match c with 
  | 0 -> return (return ())
  | c ->
    IntRevision.fork rev 
      (fun r1 -> 
         IntRevision.fork r1 (fun r -> return (IntRevision.write r iso (c-15)))
         >>|fun fr -> 
         let r_join = IntRevision.join r1 fr in                
          (IntRevision.write r_join iso (c-25)))
    >>|fun r2 ->
    let r_join = IntRevision.join rev r2 in
    IntRevision.read r_join iso
    >>|fun i ->
    assert(i=c-25);
    ignore(test r_join iso (c-25));
    ()

let c = 250000 (* should be divisible by 25 or test would never terminate*)

let () = let r = IntRevision.init () in 
  let res1 = IntRevision.create r c in
  let r1 = IntRevision.get_revision res1 
  and i1 = IntRevision.get_isolated res1 in 
  ignore(
    (test r1 i1 c)
    >>|fun  _ -> print_string("Test passed :) \n"));
  ignore(Scheduler.go()); ()

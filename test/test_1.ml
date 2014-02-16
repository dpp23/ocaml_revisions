open Core.Std
open Async.Std
open Revision

module IntRevision = Make(struct 
    type t = int
    let merge _ _ c = c
  end)

let rec test rev iso c = match c with 
  | 0 -> return (return ())
  | c -> IntRevision.fork rev (fun r -> return (IntRevision.write r iso (c-1)))
    >>|fun r2 ->
    let r_join = IntRevision.join rev r2 in
    IntRevision.read r_join iso
    >>|fun i -> 
    assert(i=c-1);
    ignore(test r_join iso (c-1));
    ()

let c = 10000

let () = let r = IntRevision.init () in 
  let res1 = IntRevision.create r c in
  let r1 = IntRevision.get_revision res1 
  and i1 = IntRevision.get_isolated res1 in 
  ignore(
  (test r1 i1 c)
  >>|fun  _ -> print_string("Test passed :) \n"));
  ignore(Scheduler.go()); ()

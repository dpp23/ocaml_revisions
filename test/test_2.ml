open Core.Std
open Async.Std
open Revision

module IntRevision = Make(struct 
    type t = int
    let merge a b c = c - b + a
  end)

let rec test rev iso c b = match c with 
  | 0 -> return (return ())
  | c ->
    let f1 = IntRevision.fork rev (fun r -> return (IntRevision.write r iso (c-10)))
    and f2 = IntRevision.fork rev (fun r -> return (IntRevision.write r iso (c-15))) in
    Deferred.both f1 f2 
    >>|fun (f1,f2)->
    if b then begin
      let r_join = IntRevision.join (IntRevision.join rev f1) f2 in
      IntRevision.read r_join iso
      >>|fun i -> 
      assert(i=c-25);
      ignore(test r_join iso (c-25) (not b));
      ()
    end
    else begin
    let r_join = IntRevision.join (IntRevision.join rev f2) f1 in
      IntRevision.read r_join iso
      >>|fun i -> 
      assert(i=c-25);
      ignore(test r_join iso (c-25) (not b));
      ()
    end

let c = 250000  (* should be divisible by 25 or test would never terminate*)

let () = let r = IntRevision.init () in 
  let res1 = IntRevision.create r c in
  let r1 = IntRevision.get_revision res1 
  and i1 = IntRevision.get_isolated res1 in 
  ignore(
    (test r1 i1 c true)
    >>|fun  _ -> print_string("Test passed :) \n"));
  ignore(Scheduler.go()); ()

open Revision
open Core.Std
open Async.Std

module IntRevision = Make(struct 
    type t = int
    let merge a b c = b + c - a
  end)

let () = 
  let r = IntRevision.init () in 
  let res1 = IntRevision.create r 1 in
  let r1 = IntRevision.get_revision res1 and i1 = IntRevision.get_isolated res1 in
  ignore( IntRevision.fork r1 (fun r -> return (IntRevision.write r i1 5)) 
          >>|(fun r2 ->
              let r3 = IntRevision.join r1 r2 in
              ignore (IntRevision.read r1 i1 >>| fun (Some(v)) -> printf "%d " v);
              ignore (IntRevision.read r3 i1 >>| fun (Some(v)) -> printf "%d " v); ()));
  never_returns (Scheduler.go ())     


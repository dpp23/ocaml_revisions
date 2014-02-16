open Revision
open Core.Std
open Async.Std

exception Flacky_code

module IntRevision = Make(struct 
    type t = string
    let merge a _ c = a ^ c 
  end)

let () = 
  let r = IntRevision.init () in 
  let res1 = IntRevision.create r "Hello" in
  let r1 = IntRevision.get_revision res1 and i1 = IntRevision.get_isolated res1 in
  ignore( IntRevision.fork r1 (fun _ -> raise Flacky_code) 
          >>|(fun r2 ->
              (* If unhandled exception is raised during the fork evaluation, it is re-raised at the time of the join *) 
              try
              let r3 = IntRevision.join r1 r2 in
              ignore (IntRevision.read r3 i1 >>| fun v -> print_string v); ()
              with
              Flacky_code -> ignore (IntRevision.read r1 i1 >>| fun v -> print_string v); ()));
  never_returns (Scheduler.go ())     


open Revision
open Core.Std

module IntIsolatable = Isolate(struct 
                                      type t = int
                                      let merge a b c = b + c - a
                               end)

module IntRevision = Revise(IntIsolatable)


let () = 
  let r = IntRevision.init () in 
    let (r1, i1) = IntRevision.create r 1 in
      let r2 = IntRevision.fork r1 (fun r -> IntRevision.write r i1 5) in
        let Some(v) = (IntRevision.read r1 i1) in
          printf "%d " v;
          let r1 = IntRevision.join r1 r2 in
            let Some(v) = (IntRevision.read r1 i1) in
            printf "%d" v   
        

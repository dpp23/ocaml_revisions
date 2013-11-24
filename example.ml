open Revision


module IntIsolatable = Isolate(struct 
                                      type t = int
                                      let merge _ _ c = c
                               end)

module IntRevision = Revise(IntIsolatable)


let () = 
  let r = IntRevision.init () in 
    let (r1, i1) = IntRevision.create 1 in
      let r2 = IntRevision.fork r1 (fun r -> IntRevision.write r i1 2) in
        printf (IntRevision.read r1 i1);
        let r1 = IntRevision.join r1 r2 in
          printf (IntRevision.read r1 i1)    
        

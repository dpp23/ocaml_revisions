open Core.Std
open Async.Std
open Revision

module IntRevision = Make(struct 
    type t = int
    let merge a b c = c - b + a
  end)


let rec create_isolated rev n iso_list= match n with 
  |0 -> return (rev, iso_list)
  |_ -> let res = IntRevision.create rev n in
    create_isolated (IntRevision.get_revision res) (n-1) ((IntRevision.get_isolated res)::iso_list)


let i = 100 (*number of isolated*)
let n = 100 (*test iterations*)

let rec write_list rev iso_list = match iso_list with
  |[] -> return rev
  |x::xs -> IntRevision.read rev x
    >>= fun c -> write_list (IntRevision.write rev x (c+1)) xs

(* validate: revision -> iso_list -> list len -> iteration *)

let rec validate rev iso_list l m = match iso_list with 
  |[] -> assert(l=0); ()
  |iso::isos -> assert(l>0);
    ignore(IntRevision.read rev iso
           >>| fun x -> assert(x = i - l + m +1));
    validate rev isos (l-1) m  

let rec test rev iso_list m = match m with 
  | 0 -> return ()
  | m ->
    IntRevision.fork rev (fun r -> write_list r iso_list)
    >>=fun fr -> 
    let r_join = IntRevision.join rev fr in
    validate r_join iso_list i (n - m + 1);
    test r_join iso_list (m-1)




let () = ignore(create_isolated (IntRevision.init()) i []
                >>= fun (rev, iso_list)-> test rev iso_list n 
                >>|fun  _ -> print_string("Test passed :) \n"));
  ignore(Scheduler.go()); ()

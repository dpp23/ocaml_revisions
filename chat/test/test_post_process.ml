open Core.Std
open Async.Std

type mes = Send of int * Time.t
          |Receive of int * Time.t
          with sexp

type kol = {id:int; time: Time.t}

let smap = ref (Map.empty ~comparator:Int.comparator)
let rmap = ref (Map.empty ~comparator:Int.comparator)

let stdin = Lazy.force Reader.stdin
exception Finish

let rec read_stream () = Reader.read_line stdin
  >>= (function 
      | `Eof -> return ()
      | `Ok line ->  let k = mes_of_sexp (Sexp.of_string line) in
                      match k with
                        |Send(i,t)->  smap := Map.add !smap ~key:i ~data:{id = i; time = t }; read_stream()
                        |Receive(i,t)-> rmap := Map.add !rmap ~key:i ~data:{id = i; time = t}; read_stream())
let first (_,b)= b
let rec wr a b um = match (a,b) with
              |([],[]) -> ()
              |(x::xs,y::ys) -> if x.id = y.id then begin
                                      print_string ((string_of_int um) ^ "," ^ (Float.to_string (Core.Span.to_float (Time.diff y.time x.time)))^ "\n");
                                      wr xs ys um
                                    end
                                    else
                                      if x.id < y.id then wr xs (y::ys) um 
                                      else wr (x::xs) ys um 
              |_-> ()

let write_result um = let s = List.sort ~cmp:(fun a b -> Int.compare a.id b.id) (first (StdLabels.List.split (Map.to_alist !smap)))
                      and r = List.sort ~cmp:(fun a b -> Int.compare a.id b.id) (first (StdLabels.List.split (Map.to_alist !rmap))) in
                         wr s r um
                         


let () =
  Command.async_basic
    ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-um" (optional_with_default 0 int)
          ~doc:" umnumber "
    )
    (fun um () -> upon (read_stream()) (fun _ -> write_result um; ignore(Shutdown.shutdown 0)); ignore(Scheduler.go());return () )
  |> Command.run

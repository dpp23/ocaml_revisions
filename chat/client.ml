open Core.Std
open Async.Std
open Messages

let regexp_cn = Str.regexp "cn *"

let id = ref 0


let register (_,r,w) = print_string "Registering\n"; Reader.read_sexp r 
                        >>|(function
                             | `Eof -> return ()
                             | `Ok s -> match command_of_sexp s with
                                                | Registered(x) -> print_string "Registered\n"; return (id := x)
                                                | _ -> return (print_string "Server is not responding, please trya agin later")  )

let send w line = if Str.string_match regexp_cn line 0 then 
                    let result = Enter( int_of_string (Str.replace_first regexp_cn "" line)) in (*TODO: catch int_of_string exception*)
                        Writer.write_sexp w (sexp_of_command result)
                  else let result = Message({timestamp = Time.now();
                                             user_id = !id;
                                             text = line;
                                             room_id = 0
                                            }) in
                          Writer.write_sexp w (sexp_of_command result)
                                              
                      


let rec sending (s, r, w) = Reader.read_line (Lazy.force Reader.stdin)
                            >>= (function
                                   | `Eof -> return ()
                                   | `Ok line -> send w line; sending (s, r, w) )
                 
       
let rec receive (s, r, w) = Reader.read_sexp r 
                            >>= (function
                                 | `Eof ->  return ()
                                 | `Ok l -> match command_of_sexp l with
                                             | Message (m) -> print_string m.text; receive (s, r, w)
                                             | _ -> receive (s, r, w) ) 


let run ~host ~port =
  let connection = Tcp.connect (Tcp.to_host_and_port host port) in
    ignore((connection >>= register) >>| (fun _ -> ignore(In_thread.run (fun ()-> connection >>= sending)); connection >>= receive)); 
    Deferred.never ()
  

let () =
  Command.async_basic
    ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-host" (optional_with_default "127.0.0.1" string)
        ~doc:" Convert to uppercase before echoing back"
      +> flag "-port" (optional_with_default 8765 int)
        ~doc:" Port to listen on (default 8765)"
    )
    (fun host port () -> run ~host ~port)
  |> Command.run

open Core.Std
open Async.Std

let listen (_,r,w) = print_string "Listening"; ignore (Reader.read_line r >>= (function
                                         | `Eof -> print_string "cEOF"; return ()
                                         | `Ok line -> print_string "cOK"; return (print_string line)));
                 Reader.read_line (Lazy.force Reader.stdin) >>| function
                                                                  | `Eof -> print_string "inEOF"; return ()
                                                                  | `Ok line -> print_string "inOK"; print_string line; return (Writer.write w (line ^ "\n" )) 
                 
       



let run ~host ~port =
  let connection = Tcp.connect (Tcp.to_host_and_port host port) in
    ignore(print_string "Ignoring"; connection >>| listen); 
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

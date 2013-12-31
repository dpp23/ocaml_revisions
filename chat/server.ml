open Core.Std
open Async.Std
open Messages

let id = ref 0

let rec serve r w = Reader.read_sexp r 
                       >>= (function
                             | `Eof -> return ()
                             | `Ok s -> match command_of_sexp s with
                                         | Message (m) -> 
                                               Writer.write_sexp w (sexp_of_command (Message({m with text = "Received: " ^ m.text}))); serve r w
                                         | _ -> serve r w )
       



let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _ r w -> id:=!id +1; Writer.write_sexp w (sexp_of_command (Registered(!id))); serve r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  Command.async_basic
    ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 8765 int)
        ~doc:" Port to listen on (default 8765)"
    )
    (fun port () -> run ~port)
  |> Command.run

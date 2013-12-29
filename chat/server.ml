open Core.Std
open Async.Std

let serve r w = print_string "Serving"; Core.Std.Unix.sleep 2; Writer.write w "Specify a username please: \n"; print_string "Send";
                Reader.read_line r >>= function
                                         | `Eof -> print_string "EOF"; return ()
                                         | `Ok line -> print_string "OK"; return (print_string line)
       



let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _addr r w -> serve r w)
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

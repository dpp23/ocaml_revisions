open Core.Std
open Async.Std
open Messages

exception Parse_error of string;;

let regexp_number = Str.regexp "[0-9][0-9]*"
let regexp_dump = Str.regexp "enter "
let regexp_leave = Str.regexp "leave [0-9][0-9]*[ ]*$"
let regexp_leave_ = Str.regexp "leave "
let regexp_send = Str.regexp "send "
let regexp_send_ = Str.regexp "send "
let regexp_promote = Str.regexp "promote [0-9][0-9]*[ ][0-9][0-9]*[ ]*$"
let regexp_promote_ = Str.regexp "promote[ ]"
let regexp_merge = Str.regexp "merge [0-9][0-9]*[ ][0-9][0-9]*[ ]*$"
let regexp_merge_ = Str.regexp "merge "
let regexp_create = Str.regexp "create [0-9][0-9]*[ ]*"
let regexp_create_ = Str.regexp "create "
let regexp_list_history = Str.regexp "list_history [0-9][0-9]*[ ][0-9][0-9]*[ ]*$"
let regexp_list_history_ = Str.regexp "list_history "
let regexp_list_rooms = Str.regexp "list_rooms[ ]*"
let regexp_help = Str.regexp  "help[ ]*"

let help = "Usage: \n
            help - list commands help\n
            send [message] - send a message to the server\n"

let id = ref 0
let stdin = Lazy.force Reader.stdin
let stdout = Lazy.force Writer.stdout

let rec register (soc,r,w) = print_string "Enter user name: "; 
  Reader.read_line (Lazy.force Reader.stdin)
  >>= (function
      | `Eof -> return ()
      | `Ok line -> Writer.write_sexp w (sexp_of_command (Register(line)));
        Reader.read_sexp r 
        >>|(function
            | `Eof -> ()
            | `Ok s -> match command_of_sexp s with
              | Registered(user_id,name) -> print_string ("Registered as " ^ name ^ " id: " 
                                                          ^ (string_of_int user_id) ^ "\n");
                id := user_id
              | Error(s) -> print_string ("Error from server: " ^ s); 
                ignore(register (soc,r,w));
                ()
              | _-> print_string "Registration failed. Try again.\n"; 
                ignore (register(soc,r,w));
                ()))                 


let parse_command line = try begin
  if Str.string_match regexp_dump line 0 then
    let m = Str.replace_first regexp_dump "" line in
    Dump_to_file(m)
  else if Str.string_match regexp_leave line 0 then
    Nop
  else if Str.string_match regexp_create line 0 then 
    Nop 
  else if Str.string_match regexp_send line 0 then
       let m = Str.replace_first regexp_send_ "" line in
       Message({timestamp = Time.now();
                                  user_id = !id;
                                  text = m
                                 })
  else if Str.string_match regexp_promote line 0 then
   Nop
  else if Str.string_match regexp_merge line 0 then
    Nop
  else if Str.string_match regexp_list_history line 0 then
   Nop
  else if Str.string_match regexp_list_rooms line 0 then 
    Nop 
  else if Str.string_match regexp_help line 0 then begin
    print_string(help); Nop end
  else raise (Parse_error "Unrecognized command! Type help for manual")
end with 
|Parse_error(s)|Failure(s) -> print_string("Error in command parser! " ^ s); Nop

let handle c = match c with 
  |Error(s) -> print_string ("Error from server! " ^ s ^ "\n")
  |_ -> ()







let rec sending (s, r, w) =  Reader.read_line stdin
  >>= (function
      | `Eof ->  return ()
      | `Ok line -> match parse_command line with
        |Nop->  sending (s,r,w)
        |result -> Writer.write_sexp w (sexp_of_command result); sending (s,r,w))

let rec receive (s, r, w) = Reader.read_sexp r 
  >>= (function
      | `Eof ->  return ()
      | `Ok l -> handle (command_of_sexp l); receive (s, r, w))


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

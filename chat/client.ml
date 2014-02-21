open Core.Std
open Async.Std
open Messages

exception Parse_error of string;;

let regexp_number = Str.regexp "[0-9][0-9]*"
let regexp_enter = Str.regexp "enter [0-9][0-9]*[ ]*$"
let regexp_enter_ = Str.regexp "enter "
let regexp_leave = Str.regexp "leave [0-9][0-9]*[ ]*$"
let regexp_leave_ = Str.regexp "leave "
let regexp_send = Str.regexp "send [0-9][0-9]* "
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
            enter [room_id - int] - enter room\n
            leave [room_id - int] - leave room\n
            send [room_id - int] [message] - send a message to a room\n
            promote [room_id] [user_id] - promote user to su in a room\n
            merge [room_id] [room_id] - merge two rooms\n
            create [room_id] - create a room\n
            list_history [room_id] [number of messages] - list last n messages from a group\n
            list_rooms - list all rooms\n"

let id = ref 0

let (rooms : chat_room_local list ref) = ref []
let room_ids = ref []
let (users : user_local list ref) = ref []

let stdin = Lazy.force Reader.stdin
let stdout = Lazy.force Writer.stdout

let find_room_by_id l id = List.find l (fun (x:chat_room_local) -> x.id = id)

let find_user_by_id l id = List.find l (fun (x:user_local) -> x.id = id)

let report s = print_string("Error: " ^ s ^ "\n")

let rec update_room_by_id (l : chat_room_local list) (n : chat_room_local) = match l with
  |[] -> l
  |x::xs -> if x.id = n.id then n::xs
    else x::(update_room_by_id xs n)

let rec update_user_by_id (l : user_local list) (n : user_local) = match l with
  |[] -> l
  |x::xs -> if x.id = n.id then n::xs
    else x::(update_user_by_id xs n)

let rec remove_room_by_id (l : chat_room_local list) id = match l with
  |[] -> l
  |x::xs -> if x.id = id then xs
    else x::(remove_room_by_id xs id)

let rec remove_user_by_id (l : user_local list) id = match l with
  |[] -> l
  |x::xs -> if x.id = id then xs
    else x::(remove_user_by_id xs id)

let add_message history message = List.sort (message::history) ~cmp:(fun a b -> Time.compare a.timestamp b.timestamp)

let print_message (m:message) =  let pretty_print name = print_string((Time.to_string m.timestamp) 
                                                                      ^ " Room: " ^ (string_of_int m.room_id) 
                                                                      ^ " From: " ^ (name)
                                                                      ^ " - " ^ m.text ^ "\n") in
  match find_user_by_id !users m.user_id with
  |None -> pretty_print "Unknown"
  |Some(u) -> pretty_print u.name

let rec remove_from_list l x = match l with 
                               |[] -> []
                               |y::ys -> if y=x then ys
                                         else y::(remove_from_list ys x)

let rec print_n_messages l n = match l with
  |[] -> ()
  |(x::xs) -> print_n_messages xs (n-1); print_message x

let list_rooms () = print_string "All rooms: \n";
  List.iter !room_ids (fun id -> print_string((string_of_int id) ^ "\n"));
  print_string "You are in rooms: \n";
  List.iter !rooms (fun room -> print_string((string_of_int room.id) ^ "\n"))

let list_history room_id n = match find_room_by_id !rooms room_id with
  |None -> print_string("Error! Room id: " ^ (string_of_int room_id) ^ " not found \n")
  |Some(room) -> print_string("History for room id: " ^ (string_of_int room_id) ^ "\n"); print_n_messages room.history n


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
                id := user_id;
                users := {id = user_id; name = name; su = false } :: !users
              | Error(s) -> print_string ("Error from server: " ^ s); 
                ignore(register (soc,r,w));
                ()
              | _-> print_string "Registration failed. Try again.\n"; 
                ignore (register(soc,r,w));
                ()))                 


let parse_command line = try begin
  if Str.string_match regexp_enter line 0 then
    Enter(int_of_string (Str.replace_first regexp_enter_ "" line), !id)
  else if Str.string_match regexp_leave line 0 then
    Leave( int_of_string (Str.replace_first regexp_leave_ "" line), !id)
  else if Str.string_match regexp_create line 0 then 
    Create( int_of_string (Str.replace_first regexp_create_ "" line), !id) 
  else if Str.string_match regexp_send line 0 then
    let split = String.lsplit2 (Str.replace_first regexp_send_ "" line) ' ' in
    match split with
      Some (room, message) -> Message({timestamp = Time.now();
                                  user_id = !id;
                                  text = message;
                                  room_id = (int_of_string room)
                                 })
    | _ -> raise (Parse_error(line))
  else if Str.string_match regexp_promote line 0 then
    let split = String.split (Str.replace_first regexp_promote_ "" line) ' ' in
    match split with
      room::[user] -> Promote(!id, int_of_string user, int_of_string room)
    |_ -> raise (Parse_error(line))
  else if Str.string_match regexp_merge line 0 then
    let split = String.split (Str.replace_first regexp_merge_ "" line) ' ' in
    match split with
      room1::[room2] -> Merge(!id, int_of_string room1, int_of_string room2)
    |_ -> raise (Parse_error(line))
  else if Str.string_match regexp_list_history line 0 then
    let split = String.split (Str.replace_first regexp_list_history_ "" line) ' ' in
    match split with
      room::[n] -> list_history (int_of_string room) (int_of_string n);Nop 
    |_ -> raise (Parse_error(line))
  else if Str.string_match regexp_list_rooms line 0 then begin
    list_rooms (); Nop end 
  else if Str.string_match regexp_help line 0 then begin
    print_string(help); Nop end
  else raise (Parse_error "Unrecognized command! Type help for manual")
end with 
|Parse_error(s)|Failure(s) -> print_string("Error in command parser! " ^ s); Nop

let handle c = match c with 
  |Message(m) -> begin match find_room_by_id !rooms m.room_id with
      |None -> report "message received for unknown room."
      |Some(r) -> rooms := update_room_by_id !rooms {r with history = add_message r.history m};
        print_message m
    end
  |Enter(room_id, user_id) -> begin if user_id = !id then () (* handled by Room(...), which is send by the server *)
      else
        match (find_room_by_id !rooms room_id, find_user_by_id !users user_id) with
        |(None, _) -> ()
        |(_, None) -> ()
        |(Some(room),Some(user)) -> 
          print_string ("User " ^ user.name ^" joined room " ^ (string_of_int room_id) ^ "\n");
          rooms := update_room_by_id !rooms {room with users = user::room.users}
    end
  |Leave(room_id, user_id) ->begin if user_id = !id then
        match find_room_by_id !rooms room_id with
        |None-> print_string ("You left room: " ^ (string_of_int room_id) ^ "\n"); 
          rooms := remove_room_by_id !rooms room_id 
        | _ -> ()
      else
        match (find_room_by_id !rooms room_id, find_user_by_id !users user_id) with
        |(None, _) -> ()
        |(_, None) -> ()
        |(Some(room),Some(user)) -> 
          print_string ("User " ^ user.name ^" left room " ^ (string_of_int room_id) ^ "\n");
          rooms := update_room_by_id !rooms {room with users = remove_user_by_id room.users user_id}
    end
  |Registered(user_id, name) -> begin match find_user_by_id !users user_id with
      |None -> print_string ("New user registered with name: " ^ name ^ " id: " 
                             ^ (string_of_int user_id) ^ "\n");
        users := {id = user_id; name = name; su = false} :: !users
      |Some(_) -> print_string ("Error! Dublicated user id: " ^ (string_of_int user_id) ^ "\n")
    end
  |Promote(_, user_id, room_id) -> begin match find_room_by_id !rooms room_id with
      |None -> ()
      |Some(room) -> begin match find_user_by_id room.users user_id with
          |None -> ()
          |Some(user) -> print_string("User " ^ user.name ^ " promoted in room " 
                                      ^ (string_of_int room_id) ^ "\n");
            if user.su then ()
            else
              rooms := update_room_by_id !rooms ({room with users = 
                                                              update_user_by_id room.users {user with su = true}})
        end
    end
  |Merge_announce(r1_id,r2_id, users) -> print_string ("Merging rooms: " ^
                                         string_of_int(r1_id) ^ " and " ^ string_of_int(r2_id) ^ "\n");
                                         begin match (find_room_by_id !rooms r1_id, find_room_by_id !rooms r2_id) with
                                           |(None,None)-> print_string ("Error! Server wants to merge two unknown groups: " ^
                                                          string_of_int(r1_id) ^ " and " ^ string_of_int(r2_id) ^ "\n");
                                           |(Some(r),_)-> print_string ("Users in room " ^ string_of_int(r1_id) ^ " :\n");
                                                              List.iter users (fun u -> print_string (u.name ^ " ");print_int(u.id); print_string "\n");
                                                              let rs = remove_room_by_id !rooms r2_id in
                                                              rooms := update_room_by_id rs ({r with users = users});
                                                              room_ids := remove_from_list !room_ids r2_id  
                                           |(None, Some(r)) ->  print_string ("Users in room " ^ string_of_int(r1_id) ^ " :\n");
                                                              List.iter users (fun u -> print_string (u.name ^ " ");print_int(u.id); print_string "\n");
                                                              let rs = remove_room_by_id !rooms r2_id in
                                                              rooms := {r with users = users; id = r1_id}::rs;
                                                              room_ids := remove_from_list !room_ids r2_id  
                                         end
    
  |Create(room_id, _) -> begin match List.find !room_ids (fun id -> id = room_id) with
      |None -> print_string("New room created. Room id: " ^ (string_of_int room_id) ^ "\n");
        room_ids := room_id::!room_ids
      |Some(_) -> print_string("Error! Room that already exists has been created. Id: " 
                               ^ (string_of_int room_id) ^ "\n")
    end
  |Room_announce(room_id) -> begin match List.find !room_ids (fun id -> id = room_id) with
      |None -> print_string("Announced room id: " ^ (string_of_int room_id) ^ "\n");
        room_ids := room_id::!room_ids
      |Some(_) -> print_string("Error! Known room announced. Id: " 
                               ^ (string_of_int room_id) ^ "\n")
    end
  |User_announce(user_id, name)-> begin match List.find !users (fun u -> u.id = user_id) with
      |None -> print_string("Announced user id: " ^ (string_of_int user_id) ^ " " ^ name ^ "\n");
        users := {id = user_id; name = name; su = false}::!users
      |Some(_) -> print_string("Error! Known user announced. Id: " 
                               ^ (string_of_int user_id) ^ "\n")
    end
  |Error(s) -> print_string ("Error from server! " ^ s ^ "\n")
  |Room(room_id, user_list) -> begin match List.find !rooms (fun room -> room_id = room.id) with
      |None -> print_string("You joined room id: " ^ (string_of_int room_id) ^ "\n");
        print_string "Users in that room are [name id]: \n";
        List.iter user_list (fun user -> print_string (user.name ^ " " ^ (string_of_int user.id) ^ "\n"));
        rooms := {id = room_id; history = []; users = user_list}::!rooms
      |Some(_) -> print_string("Error! Received entrance information for room you are already in: " 
                               ^ (string_of_int room_id) ^ "\n")
    end
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

open Core.Std
open Async.Std
open Messages

exception Horror of string

type st = {id:int; rooms: chat_room list; users: user list; last_event: command} 
let state = ref {id = 0; rooms = []; users = []}

let queue = Queue.empty

let merge a _ c = match c.command with
                    |Register (name) -> let user = List.hd c.users in
                                          print_string "Registering " ^ name ^ " as " ^ (string_of_int user.id) ^ "\n";
                                          Writer.write_sexp user.writer sexp_of_command (Registered(user.id));
                                          state:= {a with users = user::a.users};
                                          !state
                    |Message (m) -> match find_by_id a.rooms m.room_id with
                                         |None -> raise Horror("Room not found at merging point in the global state!")
                                         |Some(room) -> List.iter room.users 
                                                        (fun user -> print_string "Sending message: " ^ m.text ^ " to " ^ (string_of_int m.room_id));
                                                        Writer.write user.writer sexp_of_command (Message(m)));
                                                        match find_by_id c.rooms m.room_id with
                                                               |None -> raise Horror("Room not found at merging point in the joinee!")
                                                               |Some(room) ->
                                                                              state:={a with update_by_id a.rooms room}
                                        


module ServerRevision = Make(struct 
                                      type t = st
                                      let merge a b c = a (* TODO *)
                             end)

module SvRev = ServerRevision

let res = SvRev.create (SvRev.init()) {id = 0; rooms = []; users = []})

let staterev = ref (SvRev.get_revision res)
let iso = SvRev.get_isolated res

let find_by_id l id = List.find l (fun x -> x.id = id)
let exists_by_id l id = match find_by_id l id with
                          |None -> false
                          |_ -> true
let rec remove_by_id l id = match l with
                               |[] -> l
                               |x::xs -> if x.id = id then xs
                                         else x::(remove_by_id xs id)

let rec update_by_id l n = match l with
                               |[] -> l
                               |x::xs -> if x.id = n.id then n::xs
                                         else x::(update_by_id xs n)


let rec serve r w =  let rev2 = 
                      SvRev.fork !staterev 
                       (fun rev ->
                         Reader.read_sexp r 
                         >>= (function
                             | `Eof -> return ()
                             | `Ok s -> let event = command_of_sexp s in
                                          match event with
                                         | Register (name) -> 
                                              SvRev.read rev iso 
                                              >>| function None -> raise Horror("Error in the library, isolated not found")
                                                          |Some(st) -> if List.exists st.users (fun user -> user.name = name) then 
                                                                         Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Username " ^ name ^ "is in use."))); rev
                                                                       else SvRev.write rev iso 
                                                                                   (st with {users = {id = st.id; 
                                                                                                      name = name;
                                                                                                      su = false;
                                                                                                      writer = w;
                                                                                                      reader = r;
                                                                                                     } :: st.users;
                                                                                             id = id+1;
                                                                                             last_action = action}))
                                         | Create (room_id, id) ->
                                              SvRev.read rev iso 
                                              >>| function None -> raise Horror("Error in the library, isolated not found")
                                                          |Some(st) -> if exists_by_id st.rooms id then 
                                                                         Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Chatroom " ^ id ^ "exists."))); rev
                                                                       else 
                                                                         SvRev.write rev iso 
                                                                              (st with {rooms = { id = room_id;
                                                                                                  users = [];    
                                                                                                  history = [];
                                                                                                 } :: st.rooms;
                                                                                        last_action = action
                                                                                       }))
                                         | Enter (room_id, id) -> 
                                              SvRev.read rev iso 
                                              >>| function None -> raise Horror("Error in the library, isolated not found")
                                                           |Some(st) -> match (find_by_id st.users id, find_by_id st.rooms room_id) with
                                                                           |(None,_) -> 
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown user " ^ id))); rev
                                                                           |(_, None) ->
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown room " ^ room_id))); rev
                                                                           |(Some(u),Some(r)) -> 
                                                                                 if exists_by_id r.users u.id then
                                                                                   Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("You are already in " ^ room_id))); rev
                                                                                 else 
                                                                                   let rooms = update_by_id st.rooms 
                                                                                                {r with users = {u with su = (r.users = [])}::r.users in
                                                                                    SvRev.write rev iso 
                                                                                       (st with {rooms = rooms; last_action = action})
                                         | Leave (room_id, id) -> 
                                              SvRev.read rev iso 
                                              >>| function None -> raise Horror("Error in the library, isolated not found")
                                                           |Some(st) -> match (find_by_id st.users id, find_by_id st.rooms room_id) with
                                                                           |(None,_) -> 
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown user " ^ id))); rev
                                                                           |(_, None) ->
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown room " ^ room_id))); rev
                                                                           |(Some(u),Some(r)) -> 
                                                                                 if  not exists_by_id r.users u.id then
                                                                                   Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("You are not in " ^ room_id))); rev
                                                                                 else 
                                                                                   let rooms = update_by_id st.rooms 
                                                                                                {r with users = remove_by_id r.users u.id in
                                                                                    SvRev.write rev iso 
                                                                                       (st with {rooms = rooms; last_action = action})
                                         |Promote(admin_id, user_id, room_id) -> 
                                              SvRev.read rev iso 
                                              >>| function None -> raise Horror("Error in the library, isolated not found")
                                                           |Some(st) -> match (find_by_id st.users admin_id, find_by_id st.users user_id,
                                                                                   find_by_id st.rooms room_id) with
                                                                           |(None,_,_) -> 
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown user " ^ admin_id))); rev
                                                                           |(_,None,_) -> 
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown user " ^ user_id))); rev
                                                                           |(_, _, None) ->
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown room " ^ room_id))); rev
                                                                           |(Some(a), Some(u),Some(r)) -> 
                                                                                 match find_by_id r.users a.id with
                                                                                   |None ->
                                                                                      Writer.write_sexp w 
                                                                                         (sexp_of_command 
                                                                                           (Error("You are not in " ^ room_id))); rev
                                                                                   |Some(admin) -> if !admin.su then 
                                                                                                     Writer.write_sexp w 
                                                                                                       (sexp_of_command 
                                                                                                          (Error("You are not su in " ^ room_id))); rev
                                                                                                   else match find_by_id r.users u.id with
                                                                                                    |None ->
                                                                                                       Writer.write_sexp w 
                                                                                                         (sexp_of_command 
                                                                                                           (Error("User " ^ u.id "is not in"  ^ room_id))); rev
                                                                                                    |Some(user) ->
                                                                                                          let users = update_by_id r.users {user with su = true} in              
                                                                                                            let rooms = update_by_id st.rooms 
                                                                                                                {r with users = users} r.id in
                                                                                                                   SvRev.write rev iso 
                                                                                                                      (st with {rooms = rooms; last_action = action})
                                          

                                         | Message (m) -> 
                                              SvRev.read rev iso 
                                              >>| function None -> raise Horror("Error in the library, isolated not found")
                                                           |Some(st) -> match (find_by_id st.rooms m.room_id) with
                                                                           |None -> 
                                                                               Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("Unknown room " ^  string_of_int m.room_id))); rev
                                                                           |Some(r) -> 
                                                                                 if not exists_by_id r.users m.user_id then
                                                                                   Writer.write_sexp w 
                                                                                   (sexp_of_command 
                                                                                     (Error("You are not in " ^ string_of_int r.room_id))); rev
                                                                                 else 
                                                                                   let rooms = update_by_id st.rooms 
                                                                                                {r with history = {message with timestamp = Time.now()}::r.history in
                                                                                    SvRev.write rev iso 
                                                                                       (st with {rooms = rooms; last_action = action})
                                               
                                         | _ -> serve r w )
                      in
                        SvRev.join rev rev2
                         |> (fun rev -> SvRev.read rev iso 
                              >>| function None -> 
                                      raise Horror("Error in the library, isolated not found after join")
                                          |Some(st) -> state := st; serve r w
                                                                                             
       



let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _ r w -> serve r w)
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

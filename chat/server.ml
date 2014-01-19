open Core.Std
open Async.Std
open Messages

exception Horror of string

type st = {id:int; rooms: chat_room list; users: user list; last_event: command; last_event_time: Time.t} 
let state = ref {id = 1; rooms = []; users = []; last_event = Nop; last_event_time = Time.now()}

(*let (pipe_reader, pipe_writer) = Pipe.create()*)

let add_message history message = List.sort (message::history) ~cmp:(fun a b -> Time.compare a.timestamp b.timestamp)

let find_room_by_id l id = List.find l (fun (x:chat_room) -> x.id = id)

let find_user_by_id l id = List.find l (fun (x:user) -> x.id = id)

let user_to_user_local (u:user) = ({id = u.id; name = u.name; su = u.su}:user_local)

let exists_room_by_id l id = match find_room_by_id l id with
  |None -> false
  |_ -> true

let exists_user_by_id l id = match find_user_by_id l id with
  |None -> false
  |_ -> true

let rec remove_user_by_id (l : user list) id = match l with
  |[] -> l
  |x::xs -> if x.id = id then xs
    else x::(remove_user_by_id xs id)

let rec update_room_by_id (l : chat_room list) (n : chat_room) = match l with
  |[] -> l
  |x::xs -> if x.id = n.id then n::xs
    else x::(update_room_by_id xs n)

let rec update_user_by_id (l : user list) (n : user) = match l with
  |[] -> l
  |x::xs -> if x.id = n.id then n::xs
    else x::(update_user_by_id xs n)

let send_to_user_list l c = List.iter l (fun x -> Writer.write_sexp x.writer (sexp_of_command c))


let merger a _ c = match c.last_event with
  |Register (name) -> begin match List.hd c.users with
      |None -> raise (Horror("New user not found in joinee!"))
      |Some(us) ->
        let user = {us with id = a.id + 1} in
        print_string ("Registering " ^ name ^ " as " ^ (string_of_int user.id) ^ "\n");
        Writer.write_sexp user.writer (sexp_of_command (Registered(user.id, name)));
        List.iter a.rooms (fun room -> 
            print_string ("Inform about room " ^ (string_of_int room.id));
            Writer.write_sexp user.writer 
              (sexp_of_command (Room_announce(room.id))));
        state:= {a with users = user::a.users; id = a.id+1};
        !state
    end
  |Message (m) -> begin match find_room_by_id a.rooms m.room_id with
      |None -> raise (Horror("Room not found at merging point in the global state!"))
      |Some(room) -> print_string ("Sending message: " ^ m.text ^ " to " ^ (string_of_int m.room_id));
        send_to_user_list room.users (Message(m));
        state:={a with rooms = update_room_by_id a.rooms {room with history = add_message room.history m}};
        !state
    end
  |Promote (ad,u,g) -> begin match find_room_by_id a.rooms g with
      |None -> raise (Horror("Room not found at merging point in the global state!"))
      |Some(room) -> match find_user_by_id a.users u with
        |None -> !state
        |Some(user) -> match find_user_by_id a.users ad with
          |None -> !state
          |Some(admin) -> if not admin.su then !state
            else begin
              print_string ("Announce promotion in room " ^ (string_of_int g));
              send_to_user_list room.users (Promote (ad,u,g));
              state:={a with rooms = update_room_by_id a.rooms {room with users = update_user_by_id room.users {user with su=true}}};
              !state
            end
    end
  |Leave (room_id, id) -> begin match find_room_by_id a.rooms room_id with
      |None -> raise (Horror("Room not found at merging point in the global state!"))
      |Some(room) -> send_to_user_list room.users (Leave(room_id, id));
        print_string ("User left " ^ (string_of_int room_id));
        state:={a with rooms =  update_room_by_id a.rooms {room with users = remove_user_by_id room.users id}};
        !state
    end
  |Enter (room_id, id) -> begin match (find_room_by_id a.rooms room_id, find_room_by_id c.rooms room_id) with
      |(_,None) -> raise (Horror("Room not found at merging point in joinee!"))
      |(None,_) -> !state
      |(Some(a_room), Some(c_room)) ->begin match (find_user_by_id a_room.users id, find_user_by_id c_room.users id) with
          |(None, Some(user)) ->
            send_to_user_list a_room.users (Enter(room_id, id));
            Writer.write_sexp user.writer 
              (sexp_of_command (Room(a_room.id, List.map (user::a_room.users)      
                                       user_to_user_local))); 
            print_string ("User enter " ^ (string_of_int room_id));
            state:={a with rooms = update_room_by_id a.rooms {a_room with users = user::a_room.users}};
            !state
          |(_,None) -> raise (Horror("User not found at merging point in joinee!"))
          |(Some(_),_) -> !state
        end
    end 
  |Merge (_,_,_) -> raise (Horror("Not implemented")) (*TODO:implement*)
  |Create (room_id, id) -> begin match (find_room_by_id a.rooms room_id, find_room_by_id c.rooms room_id) with
      |(_,None) -> raise (Horror("Room not found at merging point in joinee!"))
      |(Some(_), _) -> begin match find_user_by_id a.users id with 
          |None -> !state
          |Some(user) -> Writer.write_sexp user.writer 
                           (sexp_of_command (Error("Room " ^ (string_of_int id) ^ " exists")));
            !state
        end
      |(None, Some(room)) -> 
        send_to_user_list a.users (Create (room_id, id));
        print_string ("Room created " ^ (string_of_int room_id));
        state:={a with rooms = room::a.rooms}; 
        !state
    end
  |_ -> !state         



module ServerRevision = Revision.Make(struct 
    type t = st
    let merge a b c = merger a b c 
  end)

module SvRev = ServerRevision

(*let res = SvRev.create (SvRev.init()) !state

  let staterev = SvRev.get_revision res
  let iso = SvRev.get_isolated res *)



let handle_action rev iso action r w = print_string("handle action"); match action with
  | Register (name) -> 
    SvRev.read rev iso 
    >>| (function None -> raise (Horror("Error in the library, isolated not found"))
                |Some(st) -> SvRev.write rev iso 
                               {st with users = {id = st.id; 
                                                 name = name;
                                                 su = false;
                                                 writer = w;
                                                 reader = r;
                                                } :: st.users;
                                        id = st.id+1;
                                        last_event = action;
                                        last_event_time = Time.now()})
  | Create (room_id, _) ->
    SvRev.read rev iso 
    >>| (function None -> raise (Horror("Error in the library, isolated not found"))
                |Some(st) -> SvRev.write rev iso 
                               {st with rooms = { id = room_id;
                                                  users = [];    
                                                  history = [];
                                                } :: st.rooms;
                                        last_event = action;
                                        last_event_time = Time.now()
                               })
  | Enter (room_id, id) -> 
    SvRev.read rev iso 
    >>| (function None -> raise (Horror("Error in the library, isolated not found"))
                |Some(st) -> match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
                  |(None,_) -> 
                    Writer.write_sexp w 
                      (sexp_of_command 
                         (Error("Unknown user " ^ (string_of_int id)))); rev
                  |(_, None) ->
                    Writer.write_sexp w 
                      (sexp_of_command 
                         (Error("Unknown room " ^ (string_of_int room_id)))); rev
                  |(Some(u),Some(r)) -> 
                    if exists_user_by_id r.users u.id then begin
                      Writer.write_sexp w 
                        (sexp_of_command 
                           (Error("You are already in " ^ (string_of_int room_id)))); rev end
                    else 
                      let rooms_ = update_room_by_id st.rooms 
                          {r with users = {u with su = (r.users = [])}::r.users } in
                      SvRev.write rev iso 
                        {st with rooms = rooms_; last_event = action;
                                 last_event_time = Time.now()})
  | Leave (room_id, id) -> 
    SvRev.read rev iso 
    >>| (function None -> raise (Horror("Error in the library, isolated not found"))
                |Some(st) -> match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
                  |(None,_) -> 
                    Writer.write_sexp w 
                      (sexp_of_command 
                         (Error("Unknown user " ^ (string_of_int id)))); rev
                  |(_, None) ->
                    Writer.write_sexp w 
                      (sexp_of_command 
                         (Error("Unknown room " ^ (string_of_int room_id)))); rev
                  |(Some(u),Some(r)) -> 
                    if  not (exists_user_by_id r.users u.id) then begin
                      Writer.write_sexp w 
                        (sexp_of_command 
                           (Error("You are not in " ^ (string_of_int room_id)))); rev end
                    else 
                      let rooms_ = update_room_by_id st.rooms 
                          {r with users = remove_user_by_id r.users u.id} in
                      SvRev.write rev iso 
                        {st with rooms = rooms_; last_event = action;
                                 last_event_time = Time.now()})
  |Promote(admin_id, user_id, room_id) -> 
    SvRev.read rev iso 
    >>| (function None -> raise (Horror("Error in the library, isolated not found"))
                |Some(st) -> match (find_user_by_id st.users admin_id, find_user_by_id st.users user_id,
                                    find_room_by_id st.rooms room_id) with
                |(None,_,_) -> 
                  Writer.write_sexp w 
                    (sexp_of_command 
                       (Error("Unknown user " ^ (string_of_int admin_id)))); rev
                |(_,None,_) -> 
                  Writer.write_sexp w 
                    (sexp_of_command 
                       (Error("Unknown user " ^ (string_of_int user_id)))); rev
                |(_, _, None) ->
                  Writer.write_sexp w 
                    (sexp_of_command 
                       (Error("Unknown room " ^ (string_of_int room_id)))); rev
                |(Some(a), Some(u),Some(r)) -> 
                  match find_user_by_id r.users a.id with
                  |None ->
                    Writer.write_sexp w 
                      (sexp_of_command 
                         (Error("You are not in " ^ (string_of_int room_id)))); rev
                  |Some(admin) -> if not (admin.su) then begin 
                      Writer.write_sexp w 
                        (sexp_of_command 
                           (Error("You are not su in " ^ (string_of_int room_id)))); rev end
                    else match find_user_by_id r.users u.id with
                      |None ->
                        Writer.write_sexp w 
                          (sexp_of_command 
                             (Error("User " ^ (string_of_int u.id) ^ "is not in"  ^ (string_of_int room_id)))); rev
                      |Some(user) ->
                        let users_ = update_user_by_id r.users {user with su = true} in              
                        let rooms_ = update_room_by_id st.rooms 
                            {r with users = users_} in
                        SvRev.write rev iso 
                          {st with rooms = rooms_;
                                   last_event = action; 
                                   last_event_time = Time.now()})


  | Message (m) -> 
    SvRev.read rev iso 
    >>| (function None -> raise (Horror("Error in the library, isolated not found"))
                |Some(st) -> match (find_room_by_id st.rooms m.room_id) with
                  |None -> 
                    Writer.write_sexp w 
                      (sexp_of_command 
                         (Error("Unknown room " ^  (string_of_int m.room_id)))); rev
                  |Some(r) -> 
                    if not (exists_user_by_id r.users m.user_id) then begin
                      Writer.write_sexp w 
                        (sexp_of_command 
                           (Error("You are not in " ^ (string_of_int r.id)))); rev end
                    else 
                      let rooms_ = update_room_by_id st.rooms 
                          {r with history = {m with timestamp = Time.now()}::r.history } in
                      SvRev.write rev iso 
                        {st with rooms = rooms_; last_event = action;
                                 last_event_time = Time.now()})

  | _ -> return rev 


(*let rec process_queue rev =print_string("Wait for pipe"); upon (Pipe.read pipe_reader) 
                              (function
                                | `Eof ->print_string("FAIL"); ()
                                | `Ok (action, r, w) -> print_string ("Read from Pipe"); 
                                           upon (SvRev.fork rev (fun rev -> handle rev action w r))
                                           (fun rev2 -> process_queue (SvRev.join rev rev2))) 


*)

let handle_fork action r w  = print_string("handle action"); let res = (SvRev.create (SvRev.init()) !state) in
  let iso = SvRev.get_isolated res and rev = SvRev.get_revision res in
  (SvRev.fork rev (fun rev -> handle_action rev iso action r w))
  >>| (fun rev2 -> ignore(SvRev.join rev rev2);())

let rec serve r w = print_string("Serving"); Reader.read_sexp r >>= 
  (function
    | `Eof -> print_string("Connection broken?"); return ()
    | `Ok s -> print_string("Read a sexp");let event = command_of_sexp s and st = !state in
      match event with
      | Register (name) -> 
        if List.exists st.users (fun user -> user.name = name) then begin 
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("Username " ^ name ^ "is in use."))); serve r w
        end
        else (handle_fork event r w) >>= (fun _ -> serve r w)

      | Create (room_id, _) ->
        if exists_room_by_id st.rooms room_id then begin
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("Chatroom " ^ (string_of_int room_id) ^ "exists."))); serve r w end
        else (handle_fork event r w)>>=(fun _ -> serve r w)

      | Enter (room_id, id) -> 
        begin match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
          |(None,_) -> 
            Writer.write_sexp w 
              (sexp_of_command 
                 (Error("Unknown user " ^ (string_of_int id)))); serve r w
          |(_, None) ->
            Writer.write_sexp w 
              (sexp_of_command 
                 (Error("Unknown room " ^ (string_of_int room_id)))); serve r w
          |(Some(u),Some(r_)) -> 
            if exists_user_by_id r_.users u.id then begin
              Writer.write_sexp w 
                (sexp_of_command 
                   (Error("You are already in " ^ (string_of_int room_id)))); serve r w end 
            else (handle_fork event r w) >>= (fun _ -> serve r w)
        end
      | Leave (room_id, id) -> 
        begin match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
          |(None,_) -> 
            Writer.write_sexp w 
              (sexp_of_command 
                 (Error("Unknown user " ^ (string_of_int id)))); serve r w
          |(_, None) ->
            Writer.write_sexp w 
              (sexp_of_command 
                 (Error("Unknown room " ^ (string_of_int room_id)))); serve r w
          |(Some(u),Some(r_)) -> 
            if  not (exists_user_by_id r_.users u.id) then begin
              Writer.write_sexp w 
                (sexp_of_command 
                   (Error("You are not in " ^ (string_of_int room_id)))); serve r w end
            else (handle_fork event r w) >>= (fun _ -> serve r w)
        end
      |Promote(admin_id, user_id, room_id) -> 
        begin match (find_user_by_id st.users admin_id, find_user_by_id st.users user_id,
                     find_room_by_id st.rooms room_id) with
        |(None,_,_) -> 
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("Unknown user " ^ (string_of_int admin_id)))); serve r w
        |(_,None,_) -> 
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("Unknown user " ^ (string_of_int user_id)))); serve r w
        |(_, _, None) ->
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("Unknown room " ^ (string_of_int room_id)))); serve r w
        |(Some(a), Some(u),Some(r_)) -> 
          match find_user_by_id r_.users a.id with
          |None ->
            Writer.write_sexp w 
              (sexp_of_command 
                 (Error("You are not in " ^ (string_of_int room_id)))); serve r w
          |Some(admin) -> if not admin.su then begin
              Writer.write_sexp w 
                (sexp_of_command 
                   (Error("You are not su in " ^ (string_of_int room_id)))); serve r w end
            else match find_user_by_id r_.users u.id with
              |None ->
                Writer.write_sexp w 
                  (sexp_of_command 
                     (Error("User " ^ (string_of_int u.id)^ "is not in"  ^ (string_of_int room_id)))); serve r w
              |Some(_) ->
                (handle_fork event r w) >>= (fun _ -> serve r w)
        end
      | Message (m) -> 
        begin match (find_room_by_id st.rooms m.room_id) with
          |None -> 
            Writer.write_sexp w 
              (sexp_of_command 
                 (Error("Unknown room " ^  (string_of_int m.room_id)))); serve r w
          |Some(r_) -> 
            if not (exists_user_by_id r_.users m.user_id) then begin
              Writer.write_sexp w 
                (sexp_of_command 
                   (Error("You are not in " ^ (string_of_int r_.id)))); serve r w end
            else  (handle_fork event r w) >>= (fun _ -> serve r w)
        end

      | _ -> serve r w )



let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _ r w -> serve r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  never_returns(Scheduler.go())

let () =
  Command.async_basic
    ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 8765 int)
          ~doc:" Port to listen on (default 8765)"
    )
    (fun port () -> (*ignore(process_queue staterev);*) run ~port)
  |> Command.run

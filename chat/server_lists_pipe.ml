open Core.Std
open Async.Std
open Messages

exception Horror of string

let rec add_to_list l x f = match l with
  |[] -> [x]
  |y::ys -> if((f x y) > 0 ) then 
      x::y::ys
    else
      y::(add_to_list ys x f)

module RepMessage : sig
  type t
  type a = message

  val init: unit -> t
  val add: t -> a -> t
  val filter_by_time: t -> Time.t -> a list
end =
struct
  type a = message
  type t = message list

  let init () = []
  let add l x = add_to_list l x (fun x y -> Time.compare x.timestamp y.timestamp)
  let rec filter_by_time l time = match l with
    |[] -> []
    |x::xs -> if(Time.compare x.timestamp time > -1) then x::(filter_by_time xs time)
      else []
end

module RepUser : sig
  type t
  type a = user

  val init: unit -> t
  val is_empty: t -> bool
  val add: t -> a -> t
  val find: t -> int -> a option
  val exist: t -> int -> bool
  val remove: t -> int -> t
  val send_to_users: t -> command -> unit
  val update: t -> a -> t
  val find_by_name: t -> string -> a option
  val iter: t -> (a -> unit) -> unit
  val map: t -> (a -> 'a) -> 'a list
  val exists_by_name: t -> string -> bool
end =
struct
  type a = user 
  type t = user list

  let init () = []
  let is_empty = List.is_empty 
  let add l x = x::l
  let find (l : t) id = List.find l (fun x -> x.id = id)
  let exist l id = match find l id with
    |None -> false
    |_ -> true
  let rec remove (l : t) id = match l with
    |[] -> l
    |x::xs -> if x.id = id then xs
      else x::(remove xs id)
  let send_to_users l c = List.iter l (fun x -> Writer.write_sexp x.writer (sexp_of_command c))
  let rec update (l : t) (x : a) = match l with
    |[] -> []
    |k::ks -> if x.id = k.id then x::ks
      else k::(update ks x)
  let rec find_by_name (l : t) name = match l with
    |[] -> None
    |y::ys -> if y.name = name then Some(y)
      else find_by_name ys name

  let iter (l:t) f = List.iter l f
  let map (l:t) f = List.map l f
  let exists_by_name (l:t) name = match find_by_name l name with
    |None -> false
    |_ -> true
end

type chat_room = { history : RepMessage.t;
                   users : RepUser.t;
                   id : int;
                 }

module RepRoom : sig
  type t
  type a = chat_room 

  val init: unit -> t
  val add: t -> a -> t
  val find: t -> int -> a option
  val exist: t -> int -> bool
  val remove: t -> int -> t
  val update: t -> a -> t
  val iter: t -> (a -> unit) -> unit
  val map: t -> (a -> 'a) -> 'a list
end =
struct
  type a = chat_room
  type t = chat_room list

  let init () = []
  let add l x = x::l
  let find l id = List.find l (fun (x:chat_room) -> x.id = id)
  let exist l id = match find l id with
    |None -> false
    |_ -> true
  let rec remove l id = match l with
    |[] -> l
    |x::xs -> if x.id = id then xs
      else x::(remove xs id)
  let rec update l x = match l with
    |[] -> []
    |k::ks -> if x.id = k.id then x::ks
      else k::(update ks x)
  let iter l f = List.iter l f
  let map l f = List.map l f
end


type st = {id:int; rooms: RepRoom.t; users: RepUser.t; last_event: command; last_event_time: Time.t} 
let state = {id = 1; rooms = RepRoom.init(); users = RepUser.init(); last_event = Nop; last_event_time = Time.now()}

let (pipe_reader, pipe_writer) = Pipe.create()

let add_message history message = RepMessage.add history message

let find_room_by_id l id = RepRoom.find l id

let find_user_by_id l id = RepUser.find l id

let user_to_user_local (u:user) = ({id = u.id; name = u.name; su = u.su}:user_local)

let exists_room_by_id l id = RepRoom.exist l id

let exists_user_by_id l id = RepUser.exist l id

let remove_user_by_id l id = RepUser.remove l id

let update_room_by_id l n = RepRoom.update l n

let update_user_by_id l n = RepUser.update l n


let send_to_user_list l c = RepUser.send_to_users l c


let merger a _ c = match c.last_event with
  |Register (name) -> begin match RepUser.find_by_name c.users name with
      |None -> raise (Horror("New user not found in joinee!"))
      |Some(us) ->
        let user = {us with id = a.id + 1} in
        print_string ("Registering " ^ name ^ " as " ^ (string_of_int user.id) ^ "\n");
        Writer.write_sexp user.writer (sexp_of_command (Registered(user.id, name)));
        RepUser.send_to_users a.users (Registered(user.id, name));
        RepRoom.iter a.rooms (fun room -> 
            print_string ("Inform about room " ^ (string_of_int room.id));
            Writer.write_sexp user.writer 
              (sexp_of_command (Room_announce(room.id))));
        RepUser.iter a.users (fun u -> 
            print_string ("Inform about user " ^ (string_of_int u.id));
            Writer.write_sexp user.writer 
              (sexp_of_command (User_announce(u.id, u.name))));
        {a with users = RepUser.add a.users user; id = a.id+1}
    end
  |Message (m) -> begin match find_room_by_id a.rooms m.room_id with
      |None -> raise (Horror("Room not found at merging point in the global state!"))
      |Some(room) -> print_string ("Sending message: " ^ m.text ^ " to " ^ (string_of_int m.room_id));
        send_to_user_list room.users (Message(m));
        {a with rooms = update_room_by_id a.rooms {room with history = add_message room.history m}}
    end
  |Promote (ad,u,g) -> begin match find_room_by_id a.rooms g with
      |None -> raise (Horror("Room not found at merging point in the global state!"))
      |Some(room) -> match find_user_by_id a.users u with
        |None -> a
        |Some(user) -> match find_user_by_id a.users ad with
          |None -> a
          |Some(admin) -> if not admin.su then a
            else begin
              print_string ("Announce promotion in room " ^ (string_of_int g));
              send_to_user_list room.users (Promote (ad,u,g));
              {a with rooms = update_room_by_id a.rooms {room with users = update_user_by_id room.users {user with su=true}}}
            end
    end
  |Leave (room_id, id) -> begin match find_room_by_id a.rooms room_id with
      |None -> raise (Horror("Room not found at merging point in the global state!"))
      |Some(room) -> send_to_user_list room.users (Leave(room_id, id));
        print_string ("User left " ^ (string_of_int room_id));
        {a with rooms =  update_room_by_id a.rooms {room with users = remove_user_by_id room.users id}}
    end
  |Enter (room_id, id) -> begin match (find_room_by_id a.rooms room_id, find_room_by_id c.rooms room_id) with
      |(_,None) -> raise (Horror("Room not found at merging point in joinee!"))
      |(None,_) -> a
      |(Some(a_room), Some(c_room)) ->begin match (find_user_by_id a_room.users id, find_user_by_id c_room.users id) with
          |(None, Some(user)) ->
            send_to_user_list a_room.users (Enter(room_id, id));
            Writer.write_sexp user.writer 
              (sexp_of_command (Room(a_room.id, RepUser.map (RepUser.add a_room.users user)      
                                       user_to_user_local)));
            List.iter (RepMessage.filter_by_time a_room.history c.last_event_time) 
              (fun m -> Writer.write_sexp user.writer (sexp_of_command(Message(m)))); 
            print_string ("User enter " ^ (string_of_int room_id));
            {a with rooms = update_room_by_id a.rooms {a_room with users = RepUser.add a_room.users user}}
          |(_,None) -> raise (Horror("User not found at merging point in joinee!"))
          |(Some(_),_) -> a
        end
    end 
  |Merge (_,_,_) -> raise (Horror("Not implemented")) (*TODO:implement*)
  |Create (room_id, id) -> begin match (find_room_by_id a.rooms room_id, find_room_by_id c.rooms room_id) with
      |(_,None) -> raise (Horror("Room not found at merging point in joinee!"))
      |(Some(_), _) -> begin match find_user_by_id a.users id with 
          |None -> a
          |Some(user) -> Writer.write_sexp user.writer 
                           (sexp_of_command (Error("Room " ^ (string_of_int id) ^ " exists")));
            a
        end
      |(None, Some(room)) -> 
        send_to_user_list a.users (Create (room_id, id));
        print_string ("Room created " ^ (string_of_int room_id));
        {a with rooms = RepRoom.add a.rooms room}
    end
  |_ -> a         



module ServerRevision = Revision.Make(struct 
    type t = st
    let merge a b c = merger a b c 
  end)

module SvRev = ServerRevision

let res = SvRev.create (SvRev.init()) state

let staterev = SvRev.get_revision res
let iso = SvRev.get_isolated res 



let handle_action rev iso action r w = print_string("handle action"); match action with
  | Register (name) -> 
    SvRev.read rev iso 
    >>| fun st -> SvRev.write rev iso 
      {st with users =
                 (RepUser.add st.users  
                    {id = st.id; 
                     name = name;
                     su = false;
                     writer = w;
                     reader = r;
                    });
               id = st.id+1;
               last_event = action;
               last_event_time = Time.now()}
  | Create (room_id, _) ->
    SvRev.read rev iso 
    >>| fun st -> SvRev.write rev iso 
      {st with rooms = RepRoom.add st.rooms 
                   { id = room_id;
                     users = RepUser.init();    
                     history = RepMessage.init();
                   };
               last_event = action;
               last_event_time = Time.now()
      }
  | Enter (room_id, id) -> 
    SvRev.read rev iso 
    >>| (fun st -> match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
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
                {r with users = RepUser.add r.users {u with su = (RepUser.is_empty r.users)} } in
            SvRev.write rev iso 
              {st with rooms = rooms_; last_event = action;
                       last_event_time = Time.now()})
  | Leave (room_id, id) -> 
    SvRev.read rev iso 
    >>| (fun st -> match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
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
    >>| (fun st -> match (find_user_by_id st.users admin_id, find_user_by_id st.users user_id,
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
    >>| (fun st -> match (find_room_by_id st.rooms m.room_id) with
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
                {r with history = RepMessage.add r.history {m with timestamp = Time.now()}} in
            SvRev.write rev iso 
              {st with rooms = rooms_; last_event = action;
                       last_event_time = Time.now()})

  | _ -> return rev 

let check_action event rev iso _ w = 
  SvRev.read rev iso 
  >>|fun st -> 
  match event with
  | Register (name) -> 
    if RepUser.exists_by_name st.users name then begin 
      Writer.write_sexp w 
        (sexp_of_command 
           (Error("Username " ^ name ^ "is in use."))); Nop
    end
    else event

  | Create (room_id, _) ->
    if exists_room_by_id st.rooms room_id then begin
      Writer.write_sexp w 
        (sexp_of_command 
           (Error("Chatroom " ^ (string_of_int room_id) ^ "exists."))); Nop end
    else event

  | Enter (room_id, id) -> 
    begin match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
      |(None,_) -> 
        Writer.write_sexp w 
          (sexp_of_command 
             (Error("Unknown user " ^ (string_of_int id)))); Nop
      |(_, None) ->
        Writer.write_sexp w 
          (sexp_of_command 
             (Error("Unknown room " ^ (string_of_int room_id)))); Nop
      |(Some(u),Some(r_)) -> 
        if exists_user_by_id r_.users u.id then begin
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("You are already in " ^ (string_of_int room_id)))); Nop end 
        else event
    end
  | Leave (room_id, id) -> 
    begin match (find_user_by_id st.users id, find_room_by_id st.rooms room_id) with
      |(None,_) -> 
        Writer.write_sexp w 
          (sexp_of_command 
             (Error("Unknown user " ^ (string_of_int id)))); Nop
      |(_, None) ->
        Writer.write_sexp w 
          (sexp_of_command 
             (Error("Unknown room " ^ (string_of_int room_id)))); Nop
      |(Some(u),Some(r_)) -> 
        if  not (exists_user_by_id r_.users u.id) then begin
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("You are not in " ^ (string_of_int room_id)))); Nop end
        else event
    end
  |Promote(admin_id, user_id, room_id) -> 
    begin match (find_user_by_id st.users admin_id, find_user_by_id st.users user_id,
                 find_room_by_id st.rooms room_id) with
    |(None,_,_) -> 
      Writer.write_sexp w 
        (sexp_of_command 
           (Error("Unknown user " ^ (string_of_int admin_id)))); Nop
    |(_,None,_) -> 
      Writer.write_sexp w 
        (sexp_of_command 
           (Error("Unknown user " ^ (string_of_int user_id)))); Nop
    |(_, _, None) ->
      Writer.write_sexp w 
        (sexp_of_command 
           (Error("Unknown room " ^ (string_of_int room_id)))); Nop
    |(Some(a), Some(u),Some(r_)) -> 
      match find_user_by_id r_.users a.id with
      |None ->
        Writer.write_sexp w 
          (sexp_of_command 
             (Error("You are not in " ^ (string_of_int room_id)))); Nop
      |Some(admin) -> if not admin.su then begin
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("You are not su in " ^ (string_of_int room_id)))); Nop end
        else match find_user_by_id r_.users u.id with
          |None ->
            Writer.write_sexp w 
              (sexp_of_command 
                 (Error("User " ^ (string_of_int u.id)^ "is not in"  ^ (string_of_int room_id)))); Nop
          |Some(_) ->
            event
    end
  | Message (m) -> 
    begin match (find_room_by_id st.rooms m.room_id) with
      |None -> 
        Writer.write_sexp w 
          (sexp_of_command 
             (Error("Unknown room " ^  (string_of_int m.room_id)))); Nop
      |Some(r_) -> 
        if not (exists_user_by_id r_.users m.user_id) then begin
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("You are not in " ^ (string_of_int r_.id)))); Nop end
        else event
    end
  | Merge (_,_,_) -> raise (Horror "Not implemented!")
  | _ -> Nop


let rec process_queue rev iso = Pipe.read pipe_reader 
  >>= function
  | `Eof ->print_string("FAIL"); return ()
  | `Ok (action, r, w) -> 
    print_string ("Read from Pipe");
    check_action action rev iso r w
    >>= function
    |Nop -> process_queue rev iso
    |action -> 
      SvRev.fork rev (fun rev -> handle_action rev iso action r w)
      >>=(fun rev2 -> process_queue (SvRev.join rev rev2) iso) 




let handle_to_pipe action r w  = print_string("handle fork"); ignore(Pipe.write pipe_writer (action, r, w));()

let rec serve r w = print_string("Serving"); Reader.read_sexp r >>= 
  (function
    | `Eof -> print_string("Connection broken?"); return ()
    | `Ok s -> print_string("Read a sexp");
      ignore(handle_to_pipe (command_of_sexp s) r w);
      serve r w)



let run ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _ r w -> serve r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  ignore (process_queue staterev iso); 
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

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
  type t = ((Time.t, message, Time.comparator) Map.t)

  let init () = Map.empty ~comparator:Time.comparator
  let add l x = Map.add l x.timestamp x 
  let filter_by_time l time = let (_,v) = StdLabels.List.split (Map.to_alist (Map.filter l (fun ~key ~data:_-> (Time.compare key time) > -1 ))) in
    v
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
  type t = ((int, user, Int.comparator) Map.t)

  let init () = Map.empty ~comparator:Int.comparator
  let is_empty l = Map.is_empty l
  let add (l : t) (x : a) = Map.add l x.id x 
  let find (l : t) id = Map.find l id 
  let exist l id = match find l id with
    |None -> false
    |_ -> true
  let remove (l : t) id = Map.remove l id
  let send_to_users l c = Map.iter l (fun ~key:_ ~data:x -> Writer.write_sexp x.writer (sexp_of_command c))
  let update (l : t) (x : a) = Map.add l x.id x
  let find_by_name (l : t) (name : string) =let (_,x) = StdLabels.List.split (Map.to_alist l) in 
    let rec find l name =  match l with
      |[] -> None
      |y::ys -> if y.name = name then Some(y)
        else find ys name
    in
    find x name

  let iter (l:t) f = Map.iter l (fun ~key:_ ~data -> f data)
  let map (l:t) f = let (_,x) = StdLabels.List.split(Map.to_alist (Map.map l f)) in x
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
  type t = ((int, chat_room, Int.comparator) Map.t)

  let init () = Map.empty ~comparator:Int.comparator
  let add (l : t) (x : a) = Map.add l x.id x 
  let find (l : t) id = Map.find l id 
  let exist l id = match find l id with
    |None -> false
    |_ -> true
  let remove (l : t) id = Map.remove l id
  let update (l : t) (x : a) = Map.add l x.id x                                         
  let iter (l:t) f = Map.iter l (fun ~key:_ ~data -> f data)
  let map (l:t) f = let (_,x) = StdLabels.List.split(Map.to_alist (Map.map l f)) in x
end


type st = {id:int; rooms: RepRoom.t; users: RepUser.t; last_event: command; last_event_time: Time.t} 
let state = ref {id = 1; rooms = RepRoom.init(); users = RepUser.init(); last_event = Nop; last_event_time = Time.now()}

(*let (pipe_reader, pipe_writer) = Pipe.create()*)

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
        state:= {a with users = RepUser.add a.users user; id = a.id+1};
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
              (sexp_of_command (Room(a_room.id, RepUser.map (RepUser.add a_room.users user)      
                                       user_to_user_local)));
            List.iter (RepMessage.filter_by_time a_room.history c.last_event_time) 
              (fun m -> Writer.write_sexp user.writer (sexp_of_command(Message(m)))); 
            print_string ("User enter " ^ (string_of_int room_id));
            state:={a with rooms = update_room_by_id a.rooms {a_room with users = RepUser.add a_room.users user}};
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
        state:={a with rooms = RepRoom.add a.rooms room}; 
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
        if RepUser.exists_by_name st.users name then begin 
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

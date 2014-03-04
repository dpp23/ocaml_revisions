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

let print_message (m:message) =  (Time.to_string m.timestamp) ^ " From: " ^ (string_of_int m.user_id) ^ " - " ^ m.text ^ "\n"

module RepMessage : sig
  type t
  type a = message

  val init: unit -> t
  val add: t -> a -> t
  val filter_by_time: t -> Time.t -> a list
  val merge: t -> t -> t
  val dump: t -> string -> unit Deferred.t 
end =
struct
  type a = message
  type t = ((Time.t, message, Time.comparator) Map.t)

  let init () = Map.empty ~comparator:Time.comparator
  let add l x = Map.add l x.timestamp x 
  let filter_by_time l time = let (_,v) = StdLabels.List.split (Map.to_alist (Map.filter l (fun ~key ~data:_-> (Time.compare key time) > -1 ))) in
    v
  let merge a b = Map.merge a b (fun ~key:_ x -> match x with |`Left(m)|`Right(m)|`Both(m,_) -> Some(m))
  let dump l name = let (_,v) = StdLabels.List.split (Map.to_alist l) in
                     let l = List.sort ~cmp:(fun a b -> Time.compare a.timestamp b.timestamp) v in
                      let dump = List.fold l ~init:"" ~f:(fun ac a -> ac ^ (print_message a) ^ "\n") in
                       Writer.save name dump                        
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
  val merge: t -> t -> t
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
  let merge (a:t) (b:t) = Map.merge a b (fun ~key:_ x -> match x with
      |`Left((u:a))|`Right((u:a)) -> print_int(u.id); Some(u)
      |`Both((ul:a), (_:a)) -> print_int(ul.id); Some(ul))

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
let room_id = 0
let room = {history = RepMessage.init(); users = RepUser.init(); id = room_id} 
let state = {id = 1; rooms = RepRoom.add (RepRoom.init()) room; users = RepUser.init(); last_event = Nop; last_event_time = Time.now()}


let (pipe_reader, pipe_writer) = Pipe.create()

let add_message history message = RepMessage.add history message

let find_room_by_id l id = RepRoom.find l id

let find_user_by_id l id = RepUser.find l id


let exists_room_by_id l id = RepRoom.exist l id

let exists_user_by_id l id = RepUser.exist l id

let update_room_by_id l n = RepRoom.update l n

let update_user_by_id l n = RepUser.update l n


let merger a _ c = print_string "Merging... \n"; print_string(Sexp.to_string_hum(sexp_of_command c.last_event)); match c.last_event with
  |Register (name) -> begin match RepUser.find_by_name c.users name with
      |None -> raise (Horror("New user not found in joinee!"))
      |Some(us) ->
        begin match RepUser.find_by_name a.users name with
          |Some(_) -> Writer.write_sexp us.writer (sexp_of_command ((Error("Username " ^ name ^ "is in use.")))); a
          |None ->  
            let user = {us with id = a.id + 1} in
            print_string ("Registering " ^ name ^ " as " ^ (string_of_int user.id) ^ "\n");
            Writer.write_sexp user.writer (sexp_of_command (Registered(user.id, name)));
            {a with users = RepUser.add a.users user; id = a.id+1}
        end
    end
  |Message (m) -> begin match find_room_by_id a.rooms room_id with
      |None -> raise (Horror("Room not found at merging point in the global state!"))
      |Some(room) -> print_string ("Sending message: " ^ m.text ^ " to " ^ (string_of_int room_id));
        {a with rooms = update_room_by_id a.rooms {room with history = add_message room.history m}}
    end
  |_ -> a         



module ServerRevision = Revision.Make(struct 
    type t = st
    let merge a b c = merger a b c 
  end)

module SvRev = ServerRevision

let res = SvRev.create (SvRev.init()) state

let (pipe_fork_reader, pipe_fork_writer) = Pipe.create()

let staterev_ref = ref (SvRev.get_revision res)
let iso = SvRev.get_isolated res


let handle_action rev iso action r w = print_string("handle action"); match action with
  | Register (name) -> 
    SvRev.read rev iso 
    >>| fun st -> SvRev.write rev iso 
      {st with users =
                 (RepUser.add st.users  
                    {id = st.id; 
                     name = name;
                     writer = w;
                     reader = r;
                    });
               id = st.id+1;
               last_event = action;
               last_event_time = Time.now()}
  | Message (m) -> 
    SvRev.read rev iso 
    >>| (fun st -> match (find_room_by_id st.rooms room_id) with
        |None -> 
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("Unknown room " ^  (string_of_int room_id)))); rev
        |Some(r) -> 
            let rooms_ = update_room_by_id st.rooms 
                {r with history = RepMessage.add r.history m} in
            SvRev.write rev iso 
              {st with rooms = rooms_; last_event = action;
                       last_event_time = Time.now()})
  | Dump_to_file(name) -> 
    SvRev.read rev iso 
    >>| (fun st -> match (find_room_by_id st.rooms room_id) with
        |None -> 
          Writer.write_sexp w 
            (sexp_of_command 
               (Error("Unknown room " ^  (string_of_int room_id)))); rev
        |Some(r) -> 
          ignore (RepMessage.dump (r.history) name);
          rev)
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
  | Message (_) -> 
    begin match (find_room_by_id st.rooms room_id) with
      |None -> 
        Writer.write_sexp w 
          (sexp_of_command 
             (Error("Unknown room " ^  (string_of_int room_id)))); Nop
      |Some(_) ->  event
    end
  | Dump_to_file(_) -> event
  | _ -> Nop


let rec process_queue iso = upon (Pipe.read pipe_reader) 
    (function
      | `Eof ->print_string("FAIL");  ()
      | `Ok (action, r, w) -> 
        print_string ("Read from Pipe");
        upon (check_action action !staterev_ref iso r w)
          (function
            |Nop -> process_queue iso
            |action -> ignore(SvRev.fork !staterev_ref (fun rev -> handle_action rev iso action r w) 
                              >>| fun rev -> ignore(Pipe.write pipe_fork_writer (SvRev.determine_revision rev));
                              print_string "put in fork pipe \n") ;
              print_string "Forked \n";
              process_queue iso))

let rec process_joins () = upon (Pipe.read pipe_fork_reader)
    (function
      | `Eof ->print_string("FAIL fork pipe"); ()
      | `Ok (rev : SvRev.t) -> staterev_ref := (SvRev.join !staterev_ref rev); process_joins ()) 

let handle_to_pipe action r w  = print_string("handle fork"); ignore(Pipe.write pipe_writer (action, r, w));()

let rec serve r w = print_string("Serving"); Reader.read_sexp r  
  >>=(function
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
  ignore (process_queue iso);
  ignore (process_joins ());
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

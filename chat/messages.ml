open Core.Std
open Async.Std

type message = { timestamp : Time.t;
                 user_id : int;
                 text : string;
                 room_id : int
               } with sexp

type command = Register of string (*m*)
              |Registered of int (*m*)
              |Message of message (*m*)
              |Promote of int * int * int (* admin, user, room *) (*m*)
              |Leave of int * int (*m *)
              |Enter of int * int (* m*)
              |Merge of int * int
              |Create of int * int (*m *)
              |Error of string (*m *)
              |Room of int
              |Nop
              with sexp

type user = { id : int;  
              su : bool;
              name : string;
              reader : Reader.t;
              writer : Writer.t;
            }

type chat_room = { history : message list;
                   users : user list;
                   id : int;
                 }

type user_local = { user : int;  
                    su : bool;
                  }
 
type chat_room_local = { history : message list;
                         users : user_local list;
                         id : int
                       }






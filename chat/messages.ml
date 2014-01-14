open Core.Std
open Async.Std

type message = { timestamp : Time.t;
                 user_id : int;
                 text : string;
                 room_id : int
               } with sexp

type command = Register of string (* *)
              |Registered of int (* *)
              |Message of message
              |Promote of int * int * int (* admin, user, room *)
              |Leave of int * int (* *)
              |Enter of int * int (* *)
              |Merge of int * int
              |Create of string * int (* *)
              |Error of string (* *)
              with sexp

type user = { user : int;  
              su : bool;
              name : string;
              reader : Reader.t;
              writer : Writer.t;
            }

type chat_room = { history : message list;
                   users : user list;
                   id : int;
                   name : string
                 }

type user_local = { user : int;  
                    su : bool;
                  }
 
type chat_room_local = { history : message list;
                         users : user_local list;
                         id : int
                       }






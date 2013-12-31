open Core.Std
open Async.Std

type message = { timestamp : Time.t;
                 user_id : int;
                 text : string;
                 room_id : int
               } with sexp

type command = Registered of int
              |Message of message
              |Promote of int
              |Leave of int
              |Enter of int
              |Merge of int * int
              |NewUser of int * bool
              with sexp

type user = { user : int;  
              su : bool;
              reader : Reader.t;
              writer : Writer.t;
            }

type chat_room = { history : message list;
                   users : user list;
                   id : int
                 }

type user_local = { user : int;  
                    su : bool;
                  }
 
type chat_room_local = { history : message list;
                         users : user_local list;
                         id : int
                       }






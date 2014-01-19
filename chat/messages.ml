open Core.Std
open Async.Std

type message = { timestamp : Time.t;
                 user_id : int;
                 text : string;
                 room_id : int
               } with sexp

type user_local = { id : int;  
                    su : bool;
                    name : string;
                  }with sexp

type user = { id : int;  
              su : bool;
              name : string;
              reader : Reader.t;
              writer : Writer.t;
            }
            

type command = Register of string (*m*)
              |Registered of int * string (*rm*)
              |Message of message (*mr*)
              |Promote of int * int * int (* admin, user, room *) (*rm*)
              |Leave of int * int (*mr *)
              |Enter of int * int (*mr*)
              |Merge of int * int * int (* user room room *)
              |Create of int * int (*mr *)
              |Error of string (*rm *)
              |Room of int * (user_local list) (*r*)
              |Room_announce of int (*r*)
              |Nop
              with sexp

type chat_room = { history : message list;
                   users : user list;
                   id : int;
                 }
 
type chat_room_local = { history : message list;
                         users : user_local list;
                         id : int
                       }






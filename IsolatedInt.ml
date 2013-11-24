open Isolated
open Core.Std

module IsolatedInt:Isolated = struct
  type t = int

  let init:t = 0;;
  let merge (origin:t) (current:t) (merged:t)  =  merged
  let create () = init
  let read x:t = t:int

end

 
  

open Db
    
type 'a t =
  | Empty
  | Database of 'a database

let init_state = Empty

open Db
    
type 'a t =
  | Empty
  | Database of 'a database

let init_state = Empty

let help param =
  let initialize = "placeholder" in
  let load = "placeholder" in
  let query = "placeholder" in
  let add = "placeholder" in
  let delete = "placeholder" in
  let write = "placeholder" in
  let quit = "placeholder" in
  let default =
    "The following commands are available\nhelp\ninitialize\nload\nquery\nadd\ndelete\nwrite\nquit\nTo get information about a command, use [help \"command\"]" in
  match param with
  | "initialize"::[] -> initialize
  | "load"::[] -> load
  | "query"::[] -> query
  | "add"::[] -> add
  | "delete"::[] -> delete
  | "write"::[] -> write
  | "quit"::[] -> quit
  | _ -> default

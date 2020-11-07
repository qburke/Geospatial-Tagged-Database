open Db
    
type 'a t =
  | Empty
  | Database of 'a database

let init_state = Empty

exception DatabaseAlreadyExists
  
exception NoDatabaseIntialized

let initialize st name  =
  if st != Empty then raise DatabaseAlreadyExists else
  Database (create_db name (create_element "" [""] (0.,0.)))

let add st data tags location =
  match st with
  | Database db -> create_element data tags location |> Db.add db
  | Empty -> raise NoDatabaseIntialized

let help param =
  let initialize = "[initialize name] creates a new database with the given name\n" in
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

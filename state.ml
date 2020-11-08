open Db
    
type 'a t =
  | Empty
  | Database of 'a database

let init_state = Empty

exception DatabaseAlreadyExists
  
exception NoDatabaseInitialized

let initialize st name  =
  if st != Empty then raise DatabaseAlreadyExists else
    Database (create_db name (create_element "" [""] (0.,0.)))

let is_initialized = function
  | Empty -> false
  | _ -> true

let query_elems st tags =
  match st with
  | Database db ->
    tag_search db (list_of_elements db) tags |>
    List.map (fun e -> (data_of_element e, location_of_element e, tags_of_element e))
  | Empty -> raise NoDatabaseInitialized

let add st data tags location =
  match st with
  | Database db -> create_element data tags location |> Db.add db
  | Empty -> raise NoDatabaseInitialized

let get_elems st =
  match st with
  | Database db ->
    list_of_elements db |>
    List.map (fun e -> (data_of_element e, location_of_element e, tags_of_element e))
  | Empty -> raise NoDatabaseInitialized

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

open Db
    
type 'a t =
  | Empty
  | Database of 'a database

let init_state = Empty

exception DatabaseAlreadyExists
  
exception NoDatabaseInitialized

exception DataNotFound

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

let get_tags st =
  match st with
  | Empty -> raise NoDatabaseInitialized
  | Database db ->
    list_of_reverse_index db

let delete_elem st n =
  match st with
  | Empty -> raise NoDatabaseInitialized
  | Database db ->
    let e =
      try element_of_data db n with
      | Not_found -> raise DataNotFound in
    remove db e
         


let help param =
  let initialize = "[initialize] creates a new database\n" in
  let load = "[load filename] loads a given database file\n" in
  let list = "[list] prints the elements of the database\n" in
  let tags = "[tags] prints the current tags in the database\n" in
  let query = "[query] searches the database by location and tags" in
  let add = "[add] adds an element to the database" in
  let delete = "[delete] removes an element from the database" in
  let write = "[write filename] writes the database to the given file" in
  let quit = "[quit] exits the interface" in
  let default =
    "The following commands are available\nhelp\ninitialize\nload\nquery\nadd\ndelete\nwrite\nquit\nTo get information about a command, use [help \"command\"]\n" in
  match param with
  | "initialize"::[] -> initialize
  | "load"::[] -> load
  | "list"::[] -> list
  | "tags"::[] -> tags
  | "query"::[] -> query
  | "add"::[] -> add
  | "delete"::[] -> delete
  | "write"::[] -> write
  | "quit"::[] -> quit
  | _ -> default

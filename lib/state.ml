open Db

type t =
  | Empty
  | Database of database

let init_state = Empty

exception DatabaseAlreadyExists

exception NoDatabaseInitialized

exception DataNotFound

exception TagNotFound

exception InvalidFileName

let initialize st name  =
  if st != Empty then raise DatabaseAlreadyExists else
    Database (create_db name)

let is_initialized = function
  | Empty -> false
  | _ -> true

let query_elems st v tags =
  try (match st with
      | Database db ->
        tag_search db (list_of_elements db) tags |>
        List.map (fun e -> string_of_element v e)
      | Empty -> raise NoDatabaseInitialized) with
  | Not_found -> raise TagNotFound

let add st id location tags data =
  match st with
  | Database db -> create_element id tags location data |> Db.add db
  | Empty -> raise NoDatabaseInitialized

let delete_elem st n =
  match st with
  | Empty -> raise NoDatabaseInitialized
  | Database db ->
    let e =
      try find db n with
      | Not_found -> raise DataNotFound in
    remove db e

let get_elems st v =
  match st with
  | Database db ->
    list_of_elements db |>
    List.map (fun e -> string_of_element v e)
  | Empty -> raise NoDatabaseInitialized

let get_tags st =
  match st with
  | Empty -> raise NoDatabaseInitialized
  | Database db ->
    list_of_reverse_index db

let load_db st f name=
  match st with
  | Database _ -> raise DatabaseAlreadyExists
  | Empty ->
    try Database (Db.from_json f name) with
    | _ -> raise InvalidFileName

let write_db st fmt f =
  match st with
  | Empty -> raise NoDatabaseInitialized
  | Database db -> 
    begin 
      match fmt with 
      | "rtree" -> Db.to_rtree_json db f
      | "list" -> Db.to_list_json db f
      | s -> raise (Invalid_argument s)
    end

let help param =
  let initialize = "[initialize] creates a new database\n" in
  let load = "[load] loads from a json file\n" in
  let list = "[list (verbose)] prints the elements of the database. [verbose] \
              is an optional parameter to print the json data of each element.\
              \n" in
  let tags = "[tags] prints the current tags in the database\n" in
  let query = "[query] searches the database by location and tags. [verbose] is\
               an optional parameter to print the json data of each element" in
  let add = "[add] adds an element to the database" in
  let delete = "[delete] removes an element from the database" in
  let write = "[write] writes the database to a file\n" in
  let quit = "[quit] exits the interface" in
  let default =
    "The following commands are available\nhelp\ninitialize\nload\nquery\nadd\n\
     delete\nwrite\nquit\nTo get information about a command, use [help \"\
     command\"]\n" in
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

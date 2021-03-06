open Opium
module Log = Dolog.Log

exception ParamNotFound of string
exception DbNotLoaded
exception MalformedTagList
exception MalformedJSON
exception EntryNotFound of string
exception EmptyId

(** [exn_response exn] builds the error response when [exn] is encountered. *)
let exn_response exn =
  let status, err_msg = match exn with
    | ParamNotFound p ->
      `Bad_request, "Required parameter \"" ^ p ^ "\" not found"
    | DbNotLoaded ->
      `Not_found, "Database not loaded"
    | Registry.DbAlreadyLoaded ->
      `Conflict, "Database already loaded"
    | Sys_error _ ->
      `Not_found, "Database not found"
    | MalformedTagList ->
      `Bad_request, "Tag list must be a JSON array of strings"
    | MalformedJSON ->
      `Bad_request, "Malformed JSON"
    | EntryNotFound id ->
      `Not_found, "Entry with id \"" ^ id ^ "\" not found"
    | EmptyId ->
      `Bad_request, "id cannot be an empty string"
    | Failure s when s = "float_of_string" ->
      `Bad_request, "Coordinates and radius must be floats"
    | Failure s when s = "int_of_string" ->
      `Bad_request, "Number of results must be an interger"
    | _ -> `Bad_request, "Unknown error"
  in Response.of_plain_text ~status:status err_msg
     |> Lwt.return

(** [reg] is the mutable registry of databases for the server instance *)
let reg = Registry.init ()

(** [timestamp_msg channel optype msg] is the timestamped message for [optype]
    and [msg]. *)
let timestamp_msg oc optype msg=
  let utime = Unix.time() in
  let time = utime |> Unix.localtime in
  let us, _s = modf utime in
  Printf.fprintf oc "%04d-%02d-%02d %02d:%02d:%02d.%03d %s: %s\n"
    (1900 + time.Unix.tm_year)
    (1    + time.Unix.tm_mon)
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_sec
    (int_of_float (1_000. *. us))
    optype
    msg

(** [add_log_entry name optype msg] adds a log entry for [optype] with [msg] to
    the logfile for database [name]. *)
let add_log_entry dbname optype msg = 
  let fname = dbname ^ ".log" in
  let oc = open_out_gen [Open_append; Open_creat] 0o666 fname in
  timestamp_msg oc optype msg;
  close_out oc


(** [get_req_param k req] gets the value of query param with key [k] in request
    [req]. Raises [ParamNotFound k] if param is not found in [req]. *)
let get_req_param key req =
  match Request.query key req with
  | Some k -> k
  | None -> raise (ParamNotFound key)

(** [db_from_req req] gets the identifier of the database in [req], by getting
    the value of query param "db", and returns the database from the registry.
    Raises DbNotLoaded if not in registry. *)
let db_from_req req = 
  try
    get_req_param "db" req
    |> Registry.get reg
  with
  | Not_found -> raise DbNotLoaded

(** [initialize_handler req] is the response for the /initialize endpoint with
    request [req]. *)
let initialize_handler req =
  try
    let dbname = get_req_param "name" req in
    add_log_entry dbname "INITIALIZE" "";
    dbname
    |> Registry.add_new reg
    |> Response.of_plain_text
    |> Lwt.return
  with
  | e -> exn_response e

(** [load_handler req] is the response for the /load endpoint with
    request [req]. *)
let load_handler req =
  try begin 
    let name = get_req_param "name" req
    in let fname = name ^ ".db"
    in
    Registry.load_from_file reg fname name
    |> Response.of_plain_text
    |> Lwt.return
  end with
  | e -> exn_response e

(** [tag_list_from_json json] is the list of tags from its JSON representation
    [json]. *)
let tag_list_from_json json =
  match json with
  | `List lst -> List.map
                   (fun json -> match json with
                      | `String s -> s
                      | _ -> raise MalformedTagList
                   )
                   lst
  | _ -> raise MalformedTagList

(** [tag_search_handler req] is the response for the /tag-search endpoint with
    request [req]. *)
let tag_search_handler req =
  Lwt.bind 
    (Request.to_json req)
    (fun json ->
       try begin
         let json = match json with
           | Some json -> json
           | None -> raise MalformedJSON
         in let db = db_from_req req
         in let tags = tag_list_from_json json
         in let elems = Db.tag_search db (Db.list_of_elements db) tags
         in `List (List.map
                     (fun elem -> elem |> Db.entry_of_element |> Entry.to_json)
                     elems)
            |> Yojson.Basic.to_string
            |> Yojson.Safe.from_string
            |> Response.of_json
            |> Lwt.return
       end with
       | e -> exn_response e
    )

(** [rnn_search_handler req] is the response for the /rnn endpoint with
    request [req]. *)
let rnn_search_handler req =
  Lwt.bind 
    (Request.to_json req)
    (fun json ->
       try begin
         let json = match json with
           | Some json -> json
           | None -> raise MalformedJSON
         in let db = db_from_req req
         in let x = get_req_param "center_x" req
         in let y = get_req_param "center_y" req
         in let center = (float_of_string x, float_of_string y)
         in let radius = float_of_string (get_req_param "radius" req)
         in let tags = tag_list_from_json json
         in let elems = Db.rnn_search db center radius tags
         in `List (List.map
                     (fun elem -> elem |> Db.entry_of_element |> Entry.to_json)
                     elems)
            |> Yojson.Basic.to_string
            |> Yojson.Safe.from_string
            |> Response.of_json
            |> Lwt.return
       end with
       | e -> exn_response e
    )

(** [knn_search_handler req] is the response for the /knn endpoint with
    request [req]. *)
let knn_search_handler req =
  Lwt.bind 
    (Request.to_json req)
    (fun json ->
       try begin
         let json = match json with
           | Some json -> json
           | None -> raise MalformedJSON
         in let db = db_from_req req
         in let x = get_req_param "center_x" req
         in let y = get_req_param "center_y" req
         in let center = (float_of_string x, float_of_string y)
         in let num_results = int_of_string (get_req_param "num" req)
         in let tags = tag_list_from_json json
         in let elems = Db.knn_search db num_results center tags
         in `List (List.map
                     (fun elem -> elem |> Db.entry_of_element |> Entry.to_json)
                     elems)
            |> Yojson.Basic.to_string
            |> Yojson.Safe.from_string
            |> Response.of_json
            |> Lwt.return
       end with
       | e -> exn_response e
    )

(** [add_handler req] is the response for the /add endpoint with
    request [req]. *)
let add_handler req = 
  Lwt.bind
    (Request.to_json req)
    (fun json -> 
       try begin
         let json = match json with
           | Some json -> json
           | None -> raise MalformedJSON
         in let ent = json
                      |> Yojson.Safe.to_basic
                      |> Entry.from_json
         in if (Entry.id ent) = "" then raise EmptyId
         else let db = db_from_req req
           in let dbname = get_req_param "db" req
           in let elem = Db.create_element
                  (Entry.id ent)
                  (Entry.loc ent)
                  (Entry.tags ent)
                  (Entry.to_json ent)
           in let () = Db.add db elem
           in let msg = ent |> Entry.to_json |> Yojson.Basic.to_string
           in let () = add_log_entry dbname "ADD" msg;
           in "Success"
              |> Response.of_plain_text
              |> Lwt.return
       end with
       | e -> exn_response e
    )

(** [delete_handler req] is the response for the /delete endpoint with
    request [req]. *)
let delete_handler req = 
  try 
    let db = db_from_req req
    in let dbname = get_req_param "db" req
    in let id = get_req_param "id" req
    in let ent = begin
        try Db.find db id with
        | Not_found -> raise (EntryNotFound id)
      end
    in let () = Db.remove db ent
    in let () = add_log_entry dbname "DELETE" id;
    in "Success"
       |> Response.of_plain_text
       |> Lwt.return
  with
  | e -> exn_response e

(** [write_handler req] is the response for the /write endpoint with
    request [req]. *)
let write_handler req =
  try 
    let db = db_from_req req
    in let filename = (Db.name db) ^ ".db"
    in let () = Db.to_list_json db filename in
    "Success"
    |> Response.of_plain_text
    |> Lwt.return
  with
  | e -> exn_response e

let start () =
  Log.color_off ();
  App.empty
  |> App.post "/initialize" initialize_handler
  |> App.post "/add" add_handler
  |> App.get "/tag-search" tag_search_handler
  |> App.get "/rnn" rnn_search_handler
  |> App.get "/knn" knn_search_handler
  |> App.post "/delete" delete_handler
  |> App.post "/load" load_handler
  |> App.post "/write" write_handler
  |> App.run_command



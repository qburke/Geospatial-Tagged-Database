open Opium
module Log = Dolog.Log

(* FIXME Yojson Basic vs Safe
   TODO reexamine db function overhead -- search needs db as list? remove needs to find first?
*)

exception ParamNotFound of string
exception DbNotLoaded
exception MalformedTagList
exception MalformedJSON
exception EntryNotFound of string

let exn_response exn =
  let err_msg = match exn with
    | ParamNotFound p -> "Required parameter \"" ^ p ^ "\" not found"
    | DbNotLoaded -> "Database not loaded"
    | Sys_error _ -> "Database not found"
    | MalformedTagList -> "Tag list must be a JSON array of strings"
    | MalformedJSON -> "Malformed JSON"
    | EntryNotFound id -> "Entry with id \"" ^ id ^ "\" not found"
    | _ -> "Unknown error"
  in err_msg |> Response.of_plain_text |> Lwt.return

let reg = Registry.init ()

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

let tag_list_from_json json =
  match json with
  | `List lst -> List.map
                   (fun json -> match json with
                      | `String s -> s
                      | _ -> raise MalformedTagList
                   )
                   lst
  | _ -> raise MalformedTagList

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
         in let db = db_from_req req
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
  |> App.post "/delete" delete_handler
  |> App.post "/load" load_handler
  |> App.post "/write" write_handler
  |> App.run_command



open Opium

let reg = Registry.init ()

let get_req_param key req =
  match Request.query key req with
  | Some k -> k
  | None -> failwith (key ^ " not found")

let db_from_req req = 
  get_req_param "db" req
  |> Registry.get reg

let initialize_handler req =
  get_req_param "name" req
  |> Registry.add_new reg
  |> Printf.sprintf "%s"
  |> Response.of_plain_text
  |> Lwt.return

let load_handler req =
  let name = get_req_param "name" req
  in let fname = name ^ ".db"
  in Registry.load_from_file reg fname name
     |> Printf.sprintf "%s"
     |> Response.of_plain_text
     |> Lwt.return

let tag_list_from_json json =
  match json with
  | `List lst -> List.map
                   (fun json -> match json with
                      | `String s -> s
                      | _ -> failwith "Tags must be strings"
                   )
                   lst
  | _ -> failwith "Tags must be in list"

let tag_search_handler req =
  let db = db_from_req req
  in let json_req = Request.to_json_exn req
  in Lwt.bind 
    json_req
    (fun json ->
       (try begin
          let tags = tag_list_from_json json
          in let elems = Db.tag_search db (Db.list_of_elements db) tags
          in `List (List.map
                      (fun elem -> elem |> Db.entry_of_element |> Entry.to_json)
                      elems)
             |> Yojson.Basic.to_string
        end with
        | Failure s -> Printf.printf "%s" s; s
       )
       |> Response.of_plain_text
       |> Lwt.return
    )

let add_handler req = 
  let json_req = Request.to_json_exn req
  in Lwt.bind
    json_req
    (fun json -> 
       (try begin
          let ent = json
                    |> Yojson.Safe.to_basic
                    |> Entry.from_json
          in let db = db_from_req req
          in let elem = Db.create_element
                 (Entry.id ent)
                 (Entry.loc ent)
                 (Entry.tags ent)
                 (Entry.to_json ent)
          in let () = Db.add db elem
          in "Success"
        end with
        | Failure s -> s
       )
       |> Response.of_plain_text
       |> Lwt.return
    )
(*let db = Registry.get reg (Router.param req "name")
  in let entry = Request.to_json req *) (*Lwt.bind 
                                               (Body.to_string req.body)
                                               (fun ent -> Db.add db (Entry.from_json (Yojson.Basic.from_string ent)); Lwt.return_unit)*)

let delete_handler req = 
  let db = db_from_req req
  in let id = get_req_param "id" req
  in let ent = begin
      try Db.find db id with
      | Not_found -> failwith "entry not found"
    end
  in let () = Db.remove db ent
  in "Success"
     |> Response.of_plain_text
     |> Lwt.return

let write_handler req =
  let db = db_from_req req
  in let filename = (Db.name db) ^ ".db"
  in let () = Db.to_list_json db filename
  in "Success"
     |> Response.of_plain_text
     |> Lwt.return

let print_param_handler req =
  Printf.sprintf "Hello, %s\n" (Router.param req "name")
  |> Response.of_plain_text
  |> Lwt.return

let start () =
  App.empty
  |> App.get "/hello/:name" print_param_handler
  |> App.post "/initialize" initialize_handler
  |> App.post "/add" add_handler
  |> App.get "/tag-search" tag_search_handler
  |> App.post "/delete" delete_handler
  |> App.post "/load" load_handler
  |> App.post "/write" write_handler
  |> App.run_command
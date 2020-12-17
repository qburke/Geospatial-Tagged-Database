open Opium

let reg = Registry.init ()

(* query, add, delete *)

let initialize_handler req = 
  Printf.sprintf "%s" (Registry.add_new reg (Router.param req "name"))
  |> Response.of_plain_text
  |> Lwt.return

let add_handler req = 
  let json_req = Request.to_json_exn req
  in Lwt.bind
    json_req
    (fun json -> 
       (try begin
          let ent = json
                    |> Yojson.Safe.to_basic
                    |> Entry.from_json
          in let db = Registry.get reg (Router.param req "name")
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

let tag_search_handler req =
  let db = Registry.get reg (Router.param req "name")
  in let tag = Router.param req "tag"
  in let elems = Db.tag_search db (Db.list_of_elements db) [tag]
  in `List (List.map
              (fun elem -> elem |> Db.entry_of_element |> Entry.to_json)
              elems)
     |> Yojson.Basic.to_string
     |> Response.of_plain_text
     |> Lwt.return

let print_param_handler req =
  Printf.sprintf "Hello, %s\n" (Router.param req "name")
  |> Response.of_plain_text
  |> Lwt.return

let start () =
  App.empty
  |> App.get "/hello/:name" print_param_handler
  |> App.get "/initialize/:name" initialize_handler
  |> App.get "/add/:name" add_handler
  |> App.get "/tag-search/:name/:tag" tag_search_handler
  |> App.run_command
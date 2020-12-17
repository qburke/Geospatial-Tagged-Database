open Opium

let reg = Registry.init ()

(* query, add, delete *)

let initialize_handler req = 
  Printf.sprintf "%s" (Registry.add_new reg (Router.param req "name"))
  |> Response.of_plain_text
  |> Lwt.return

let add_handler _req = failwith "unimplemented"
(*let db = Registry.get reg (Router.param req "name")
  in let entry = Request.to_json req *) (*Lwt.bind 
                                               (Body.to_string req.body)
                                               (fun ent -> Db.add db (Entry.from_json (Yojson.Basic.from_string ent)); Lwt.return_unit)*)

let print_param_handler req =
  Printf.sprintf "Hello, %s\n" (Router.param req "name")
  |> Response.of_plain_text
  |> Lwt.return

let start () =
  App.empty
  |> App.get "/hello/:name" print_param_handler
  |> App.get "/initialize/:name" initialize_handler
  |> App.run_command
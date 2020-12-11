open Opium.Std

(* initialize, query, add, delete *)

let print_param_handler req =
  Printf.sprintf "Hello, %s\n" (Router.param req "name")
  |> Response.of_string_body
  |> Lwt.return

let start () =
  App.empty
  |> App.get "/hello/:name" print_param_handler
  |> App.run_command
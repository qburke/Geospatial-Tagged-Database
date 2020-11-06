(** [open_interface] starts the cli *)
let open_interface =
  let open State in
  let open Command in
  let rec loop st =
    print_string "> ";
    try match read_line () |> parse  with
      | Quit -> print_endline "Goodbye"; exit 0;
      | Help cmd -> help cmd |> print_endline; loop st
      | _ -> loop st
    with
    | Empty | Malformed ->
      print_endline "Invalid command\nPress enter to continue...";
      match read_line () with _ -> print_string "";
        loop st;
  in
  init_state |> loop

  let main () =
    ANSITerminal.();
    open_interface

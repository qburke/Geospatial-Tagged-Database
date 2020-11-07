exception InvalidInput
  
 
(** [open_interface] starts the cli *)
let open_interface =
  let open State in
  let open Command in          
  let rec loop st =
    print_string "> ";
    try
      try match read_line () |> parse  with
        | Initialize [name] -> initialize st name |> loop;
        | Add ->
           let split_str str =
             str |> String.trim |> String.split_on_char ' '|>
             List.filter (fun x -> x = "" |> not) in
           let validate = function
             | x::y::[] ->
               (try (float_of_string x, float_of_string y) with
                | Failure _ -> raise InvalidInput)
             | _ -> raise InvalidInput in
           let parse_name =
             print_endline "Enter label. Spaces are prohibited";
             match read_line () |> split_str with
             | s::[] -> s
             |  _-> raise InvalidInput in
           let parse_tags =
             print_endline "Enter list of tags seperated by spaces";
             match read_line () |> split_str with
             | [] -> raise InvalidInput
             | x::xs -> (x::xs) in               
           let parse_location  =
             print_endline "Enter two coordinates seperated by a space";
             read_line () |> split_str |> validate in
           add st parse_name parse_tags parse_location;
           loop st
        | Quit -> print_endline "Goodbye"; exit 0;
        | Help cmd -> help cmd |> print_endline; loop st
        | _ -> loop st
      with
      | Empty | Malformed ->
        print_endline "Invalid command\nPress enter to continue...";
        match read_line () with _ -> print_string "";
          loop st;
    with
    | DatabaseAlreadyExists ->
      (print_endline "Cannot initialize multiple databases\nPress enter to continue...";
      match read_line () with _ -> print_string "";
        loop st;)
    | NoDatabaseIntialized ->
      (print_endline "No database selected\nPress enter to continue...";
       match read_line () with _ -> print_string "";
         loop st;)
    | InvalidInput ->
      print_endline "Invalid Input\nPress enter to continue...";
      loop st;                                                     
  in
  init_state |> loop

  let main () =
    ANSITerminal.();
    open_interface

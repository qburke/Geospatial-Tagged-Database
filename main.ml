exception InvalidInput
  
let split_str str =
      str |> String.trim |> String.split_on_char ' '|>
      List.filter (fun x -> x = "" |> not) 

let parse_add () st =
  let open State in
  if is_initialized st |> not then raise NoDatabaseInitialized else
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
      | [] | [""] -> raise InvalidInput
      | x::xs -> (x::xs) in               
    let parse_location  =
      print_endline "Enter two coordinates seperated by a space";
      read_line () |> split_str |> validate in
    (parse_name, parse_tags, parse_location)

let parse_query () st =
  let open State in
  if is_initialized st |> not then raise NoDatabaseInitialized else
    print_endline "Enter list of tags seperated by states";
  match read_line () |> split_str with
  | [] | [""] -> raise InvalidInput
  | xs -> xs     

let print_elems () es =
  print_endline "Name | Location | Tags";
  print_endline "-----------------------";
  let rec aux = function
    | [] -> ignore 0;
    | (name,(x,y),tags)::xs ->
      Printf.printf "%s | %f, %f | %s\n" name x y (String.concat " " tags);
      Printf.printf "-----------------------\n";
      aux xs; in
  aux es

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
          let (name,tags,location) = parse_add () st in
          add st name tags location;
          loop st
        | Query ->
          parse_query () st |> query_elems st |> print_elems (); loop st
        | List -> get_elems st |> print_elems (); loop st
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
    | NoDatabaseInitialized ->
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

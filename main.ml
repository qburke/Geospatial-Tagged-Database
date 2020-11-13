exception InvalidInput
module Log = Dolog.Log

let split_str str =
  str |> String.trim |> String.split_on_char ' '|>
  List.filter (fun x -> x = "" |> not) 

let parse_init () st =
  let open State in
  if is_initialized st then raise DatabaseAlreadyExists else
    (print_endline "Enter name. Spaces are prohibited";
     match read_line () |> split_str with
     | s::[] -> s
     | _ -> raise InvalidInput)

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

let parse_delete () st =
  let open State in
  if is_initialized st |> not then raise NoDatabaseInitialized else
    (print_endline "Enter the element to be removed";
     match read_line () |> split_str with
     | e::[] -> e
     | _ -> raise InvalidInput)

let parse_query () st =
  let open State in
  if is_initialized st |> not then raise NoDatabaseInitialized else
    print_endline "Enter list of tags seperated by states";
  match read_line () |> split_str with
  | [] | [""] -> raise InvalidInput
  | xs -> xs     

let print_elems () es =
  print_endline "ID | Location | Tags";
  print_endline "-----------------------";
  let rec aux = function
    | [] -> ignore 0;
    | (name,(x,y),tags)::xs ->
      Printf.printf "%s | %f, %f | %s\n" name x y (String.concat " " tags);
      Printf.printf "-----------------------\n";
      aux xs; in
  aux es

let print_tags () ts =
  print_endline "Tags:";
  print_endline "-----------------------";
  let rec aux = function
    | [] -> ignore 0;
    | x::xs ->
      Printf.printf "%s\n" x;
      aux xs; in
  aux ts

let time (u:unit) (m:string) : unit = 
  let s = Unix.gettimeofday () in
  u;
  let e = Unix.gettimeofday () in
  let elapsed = (e -. s)*.(10.**6.) in
  Log.info "%s in %.2f us" m elapsed

(** [open_interface] starts the cli *)
let open_interface =
  Log.set_log_level Log.INFO;
  Log.color_on ();
  Log.set_output stdout;
  let open State in
  let open Command in          
  let rec loop st =
    print_string "> ";
    try
      try match read_line () |> parse  with
        | Initialize ->
          let name = parse_init () st in
          let init = initialize st name in 
          Log.info "Initialized DB: %s" name;
          loop init
        | Load filename -> failwith "unimplemented"
        | List -> get_elems st |> print_elems (); loop st
        | Tags -> get_tags st |> print_tags (); loop st
        | Query ->
          let queries = parse_query () st in
          time (queries |> query_elems st |> print_elems ()) ("Query complete in"); 
          loop st
        | Add ->
          let (id,tags,location) = parse_add () st in
          time (add st id tags location `Null) ("Added item "^id);
          loop st
        | Delete -> parse_delete () st |> delete_elem st; loop st
        | Write filename -> failwith "unimplemented"

        | Quit -> print_endline "Goodbye"; exit 0;
        | Help cmd ->
          help cmd |> print_endline;
          print_endline "Press enter to continue...";
          match read_line () with _ -> print_string "";
            loop st;
      with
      | Empty | Malformed ->
        Log.error "Invalid command\nPress enter to continue...";
        match read_line () with _ -> print_string "";
          loop st;
    with
    | DatabaseAlreadyExists ->
      (Log.error "Cannot initialize multiple databases\nPress enter to continue...";
       match read_line () with _ -> print_string "";
         loop st;)
    | NoDatabaseInitialized ->
      (Log.error "No database selected\nPress enter to continue...";
       match read_line () with _ -> print_string "";
         loop st;)
    | InvalidInput ->
      Log.error "Invalid Input\nPress enter to continue...";
      loop st;                                                     
  in
  init_state |> loop

let main () =
  ANSITerminal.();
  open_interface

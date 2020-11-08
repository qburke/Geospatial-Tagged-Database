type object_phrase = string list

type search_parameter = string list

type command =
  | Help of object_phrase
  | Initialize of object_phrase
  | Load of object_phrase
  | Query 
  | List
  | Add
  | Delete of object_phrase
  | Write of object_phrase
  | Quit

exception Empty

exception Malformed

let parse str =
  if str = "" then raise Empty else
    let parse_cmd = function
      | [] | [""] -> raise Empty
      | "help"::xs -> Help xs
      | "initialize"::name::[] -> Initialize [name]
      | "initialize"::_ -> raise Malformed
      | "load"::[] -> raise Malformed
      | "load"::xs -> Load xs
      | "query"::[] -> Query
      | "query"::_ -> raise Malformed
      | "add"::[] -> Add
      | "add"::_ -> raise Malformed
      | "list"::[] -> List
      | "list"::_ -> raise Malformed
      | "delete"::[] -> raise Malformed
      | "delete"::xs -> Delete xs
      | "write"::xs -> Write xs
      | "quit"::[] -> Quit
      | "quit"::_ | _ -> raise Malformed in
    String.trim str |>
    String.split_on_char ' ' |>
    List.filter (fun x -> x = "" |> not) |>
    parse_cmd

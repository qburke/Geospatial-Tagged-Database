type object_phrase = string list

type search_parameter = string list

type command =
  | Help of object_phrase
  | Initialize
  | Load
  | Query of object_phrase
  | List of object_phrase
  | Tags
  | Add
  | Delete 
  | Write 
  | Quit

exception Empty

exception Malformed

let parse str =
  if str = "" then raise Empty else
    let parse_cmd = function
      | [] | [""] -> raise Empty
      | "help"::xs -> Help xs
      | "initialize"::[] -> Initialize
      | "load"::[] -> Load
      | "query"::xs -> Query xs
      | "add"::[] -> Add
      | "list"::xs -> List xs
      | "tags"::[] -> Tags
      | "delete"::[] -> Delete
      | "write"::[] -> Write
      | "quit"::[] -> Quit
      | "initialize"::_ -> raise Malformed
      | _ -> raise Malformed in
    String.trim str |>
    String.split_on_char ' ' |>
    List.filter (fun x -> x = "" |> not) |>
    parse_cmd

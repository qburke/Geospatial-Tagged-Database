type t = (string, Db.database) Hashtbl.t

let init () = Hashtbl.create 8

let load_from_file r f name =
  let db = Db.from_json f name in
  let rec loop i name =
    if Hashtbl.mem r name then loop (i+1) name
    else name ^ string_of_int i
  in
  if Hashtbl.mem r name
  then
    let name' = loop 1 name
    in let () = Hashtbl.add r name' db
    in name'
  else
    let () = Hashtbl.add r name db
    in name

let add_new r name =
  let db = Db.create_db name
  in let () =  Hashtbl.add r name db
  in name

let get = Hashtbl.find
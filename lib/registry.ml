type t = (string, Db.database) Hashtbl.t

exception DbAlreadyLoaded

let init () = Hashtbl.create 8

let load_from_file r f name =
  let db = Db.from_json f name in
  if Hashtbl.mem r name
  then
    raise DbAlreadyLoaded
  else
    let () = Hashtbl.add r name db
    in name

let add_new r name =
  let db = Db.create_db name
  in let () =  Hashtbl.add r name db
  in name

let get = Hashtbl.find
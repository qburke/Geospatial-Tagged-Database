type element = Entry.t

type spatial_data = (element, int) Hashtbl.t

type tag_collection = (element, int) Hashtbl.t

type reverse_index = (string, tag_collection) Hashtbl.t

type database = {
  name : string;
  elements : (string, element) Hashtbl.t;
  rTree : Rtree.t;
  tag_index : reverse_index;
}

let create_element = Entry.manual

let data_of_element = Entry.data

let tags_of_element = Entry.tags

let location_of_element = Entry.mbr

let create_db name : database =
  let new_elements = Hashtbl.create 1000 in
  let new_rTree = Rtree.empty () in
  let new_tag_index = Hashtbl.create 1000 in
  {name = name; elements = new_elements;
   rTree = new_rTree; tag_index = new_tag_index}

let list_of_reverse_index db =
  Hashtbl.fold (fun k _ acc -> k::acc) db.tag_index [] |>
  List.sort compare

let list_of_tag_collection db tag =
  let tc = try Hashtbl.find db.tag_index tag with
    | Not_found -> failwith "Database does not contain tag" in
  Hashtbl.fold (fun k _ acc -> k::acc) tc []

let list_of_elements db =
  Hashtbl.fold (fun _ v acc ->v::acc) db.elements []

let add db e =
  if Hashtbl.mem db.elements (Entry.id e) then failwith "No duplicates" else
    let get_tag_collection ri tag : tag_collection =
      if Hashtbl.mem ri tag then Hashtbl.find ri tag
      else (Hashtbl.create 1000 |> Hashtbl.add ri tag; Hashtbl.find ri tag) in
    let rec add_to_index data = function
      | [] -> ignore(0);
      | x::xs ->
        Hashtbl.replace (get_tag_collection db.tag_index x) data 0;
        add_to_index data xs in
    e |> Entry.tags |> add_to_index e;
    Hashtbl.replace  db.elements (Entry.id e) e;
    Rtree.add e db.rTree

let tag_search db objects tags =
  let ri_lookup tag elem =
    let tag_set = Hashtbl.find db.tag_index tag in
    Hashtbl.mem tag_set elem in
  let filter_func elem =
    List.fold_left
      (fun acc tag-> if ri_lookup tag elem then acc else false) true tags in
  List.filter filter_func objects

let from_json f name =
  let db = create_db name in
  f
  |> Yojson.Basic.from_file 
  |> Yojson.Basic.Util.to_list
  |> List.iter (fun j -> add db (Entry.from_json j))
type 'a element = {
  data : 'a;
  tags : string list;
  location : Point.t;
}

type 'a spatial_data = ('a element, int) Hashtbl.t

type 'a tag_collection = ('a element, int) Hashtbl.t

type 'a reverse_index = (string, 'a tag_collection) Hashtbl.t

type 'a database = {
  name : string;
  elements : ('a, int) Hashtbl.t;
  rTree : 'a element Rtree.t;
  tag_index : 'a reverse_index;
}

let create_element data tags location =
  {data = data; tags = tags; location = location}

let data_of_element e =
  match e with
  | {data; _} -> data

(* Hash-tables have no specific type by default when created. Adding in and 
   deleting init_val causes the hash-table to take the type of init_val *)
let create_db name init_val : 'a database =
  let new_elements = Hashtbl.create 1000 in
  let new_rTree = Rtree.new_tree init_val.location init_val in
  let new_tag_index = Hashtbl.create 1000 in
  let new_tag_collection = Hashtbl.create 1000 in
  Hashtbl.add new_elements init_val.data 0;
  Hashtbl.add new_tag_collection init_val 0;
  Hashtbl.add new_tag_index "nil" new_tag_collection;
  Hashtbl.remove new_elements init_val.data;
  Hashtbl.remove new_tag_index "nil";
  {name = name; elements = new_elements;
   rTree = new_rTree; tag_index = new_tag_index}

let list_of_reverse_index db =
  Hashtbl.fold (fun k _ acc -> k::acc) db.tag_index [] |>
  List.sort compare

let list_of_tag_collection db tag =
  let tc = try Hashtbl.find db.tag_index tag with
    | Not_found -> failwith "Database does not contain tag" in
  Hashtbl.fold (fun k _ acc -> k::acc) tc []

let add db e =
  if Hashtbl.mem db.elements e.data then failwith "No duplicates" else
    let get_tag_collection ri tag : 'a tag_collection =
      if Hashtbl.mem ri tag then Hashtbl.find ri tag
      else (Hashtbl.create 1000 |> Hashtbl.add ri tag; Hashtbl.find ri tag) in
    let rec add_to_index data = function
      | [] -> ignore(0);
      | x::xs ->
        Hashtbl.replace (get_tag_collection db.tag_index x) data 0;
        add_to_index data xs in
    e.tags |> add_to_index e;
    Hashtbl.replace  db.elements e.data 0;
    Rtree.add e.location e db.rTree

let tag_search db objects tags =
  let ri_lookup tag elem =
    let tag_set = Hashtbl.find db.tag_index tag in
    Hashtbl.mem tag_set elem in
  let filter_func elem =
    List.fold_left
      (fun acc tag-> if ri_lookup tag elem then acc else false) true tags in
  List.filter filter_func objects

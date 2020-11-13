type t = {
  id : string;
  mbr : Rect.t;
  tags : string list;
  data : Yojson.Basic.t
}

let str_of_json err = function
  | `String s -> s
  | _ -> failwith err

let extract_coord coord json = match List.assoc_opt coord json with
  | Some (`Float c) -> c
  | Some _ -> failwith (coord ^ " is not a float")
  | None -> failwith (coord ^ " coord not provided")

let extract_point point json = match List.assoc_opt point json with
  | Some (`Assoc coords) -> begin
      let x = extract_coord "x" coords in
      let y = extract_coord "y" coords in
      (x,y)
    end
  | Some _ -> failwith (point ^ " is not an object")
  | None -> failwith (point ^ " corner not provided")

let from_json json =
  let assoc_list = match json with
    | `Assoc lst -> lst
    | _ -> failwith "Invalid JSON" in
  let id = match List.assoc_opt "id" assoc_list with
    | Some s -> str_of_json "id is not a string" s
    | None -> failwith "id not provided" in
  let mbr = match List.assoc_opt "mbr" assoc_list with
    | Some (`Assoc rect) -> begin
        let ll = extract_point "ll" rect in
        let ur = extract_point "ur" rect in
        (ll, ur)
      end
    | Some _ -> failwith "mbr is not an object"
    | None -> failwith "mbr not provided" in
  let tags = match List.assoc_opt "tags" assoc_list with
    | Some (`List lst) -> List.map (str_of_json "tags is not a list of strings") lst
    | Some `Null -> []
    | Some _ -> failwith "tags is not a list of strings or null"
    | None -> failwith "tags not provided" in
  {
    id = id;
    mbr = mbr;
    tags = tags;
    data = json
  }

let manual id mbr tags data = 
  let addtl_data = match data with
    | `Assoc lst -> lst
    | `Null -> []
    | _ -> failwith "JSON data provided is not an object nor null"
  in 
  {
    id = id;
    mbr = mbr;
    tags = tags;
    data = `Assoc (
        ("id", `String id) ::
        ("mbr", Rect.to_json mbr) ::
        ("tags", `List (List.map (fun s -> `String s) tags)) ::
        addtl_data);
  }

let id ent = ent.id

let mbr ent = ent.mbr

let tags ent = ent.tags

let data ent = ent.data

let to_json ent = ent.data

let to_string v ent = ""
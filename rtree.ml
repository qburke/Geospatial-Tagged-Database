open Point
open Rect

let max_boxes = 8
let max_entries = 8

(*
  Type requirements:
  * access parent
  * mutable bounding box for modifications
*)
type 'a t = {
  mutable parent: 'a t option;
  mutable mbr: Rect.t;
  mutable children: [`Node of 'a t list | `Entry of 'a]
}

let empty_leaf par = {
  parent = Some par;
  mbr = Rect.empty;
  children = `Entry None;
}

let empty emp = {
  parent = None;
  mbr = Rect.empty;
  children = `Node [];
}

let new_tree p x = 
  let root = {
    parent = None;
    mbr = Rect.empty;
    children = `Node [];
  } in root.children <- `Node( {
      parent = Some root;
      mbr = Rect.of_point p;
      children = `Entry x
    } :: []);
  root

let parent (n : 'a t) : 'a t =
  match n.parent with
  | Some m -> m
  | None -> n

let children (n : 'a t) : 'a t list =
  match n.children with
  | `Node lst -> lst
  | `Entry _ -> failwith "Entry does not have children"

let value (n : 'a t) : 'a =
  match n.children with
  | `Node _ -> failwith "Node does not have a value"
  | `Entry e -> e

let node_append box n =
  match n.children with 
  | `Node lst -> n.children <- `Node (box :: lst)
  | `Entry _ -> failwith "Cannot append to an Entry"

(**[node_remove node entry] removes an [entry] from a [node]. *)
let node_remove node entry =
  List.filter (fun el -> el != entry) (children node)

let mbr_of_children (n : 'a t) : Rect.t list =
  List.map (fun c -> c.mbr) (children n)

let compare_mbr_x (compare_x : bool) (x1: 'a t) (x2 : 'a t) : int = 
  let p1 = fst x1.mbr in 
  let p2 = fst x2.mbr in
  let c1 = if compare_x then fst p1 else snd p1 in
  let c2 = if compare_x then fst p2 else snd p2 in
  Stdlib.compare c1 c2

let split_at (n : int) (lst : 'a list) : ('a list * 'a list) = 
  let rec helper n lst acc = 
    if n = 0 then (acc, lst) else
      match lst with
      | [] -> (acc, [])
      | h :: t -> helper (n - 1) t (h::acc) in
  helper n lst []

let sort_subtree_x (lst: 'a t list) (sort_x: bool) : 'a t list = 
  List.sort (compare_mbr_x sort_x) lst

let ceil_int n  = n |> ceil |> int_of_float

(* [split n] is the result of splitting [n]  *) 
let split (n : 'a t list) : ('a t list * 'a t list) =
  let m = float_of_int (List.length n) in
  let sorted_lsts = [sort_subtree_x n true; sort_subtree_x n false] in
  let start_idx = ceil_int (0.25 *. float_of_int max_boxes) in
  let end_idx = ceil_int (m -. 0.25 *. float_of_int max_boxes) in
  let min_perimeter_sum = ref max_float in
  let min_split = ref (n, n) in
  for j = 0 to 1 do
    for i = start_idx to end_idx do
      let s, s' = split_at i (List.nth sorted_lsts j) in 
      let mbr_s = s |> List.map (fun x -> x.mbr) |> Rect.mbr_of_list in 
      let mbr_s' = s' |> List.map (fun x -> x.mbr) |> Rect.mbr_of_list in
      let new_perimeter_sum = (Rect.perimeter mbr_s) +. (Rect.perimeter mbr_s') 
      in
      if new_perimeter_sum < !min_perimeter_sum then begin
        min_perimeter_sum := new_perimeter_sum; 
        min_split := (s, s')
      end
    done;
  done;
  !min_split 

let node_with_children n c_lst =
  let node = {
          parent = Some n;
          mbr = Rect.empty;
          children = `Node c_lst;
  } in
  List.iter (fun c -> c.parent <- Some node) c_lst;
  node.children <- `Node c_lst;
  node

(* [handle_overflow n] splits [n] into two bounding boxes n and n', updating the
   children of n and its MBR, and adding n' to their shared parent.*)
let rec handle_overflow (n : ('a t)) : unit =
  (* split the children of n to be n and n'*)
  let u, u' = n |> children |> split in
  if n.parent = None then
    (* update children of n to be first result of split *)
    let n' = node_with_children n u in
    (* create new node n' around second result of split *)
    let n'' = node_with_children n u' in
    (* update bounding box of n'' *)
    n''.mbr <- n'' |> mbr_of_children |> Rect.mbr_of_list;
    (* update bounding box of n' *)
    n'.mbr <- n' |> mbr_of_children |> Rect.mbr_of_list; (* TODO factor out *)
    (* add new node to parent *)
    n.children <- `Node (n' :: n'' :: []);
    (* update bounding box of parent*)
    n.mbr <- n |> mbr_of_children |> Rect.mbr_of_list;
  else
    let w = parent n in
    (* update children of n to be first result of split *)
    n.children <- `Node u;
    (* update bounding box of n *)
    n.mbr <- n |> mbr_of_children |> Rect.mbr_of_list;
    (* create new node n' around second result of split *)
    let n' = node_with_children w u' in
    (* update bounding box of n' *)
    n'.mbr <- n' |> mbr_of_children |> Rect.mbr_of_list;
    (* add new node to parent *)
    w.children <- `Node (n' :: children w);
    (* update bounding box of parent *)
    w.mbr <- w |> mbr_of_children |> Rect.mbr_of_list;
    (* handle_overflow if necessary*)
    if w |> children |> List.length > max_boxes then
      handle_overflow w

(* [choose_subtree p n] is the child of [n] that requires the minimum increase
   in perimeter to cover [p]. The minimum increase in area is the tie-breaker.*)
let choose_subtree (p : ('a t)) (n : ('a t)) : 'a t =
  let enlarge_child c =
    Rect.(c.mbr |> enlargement_rect p.mbr |> area, c)
  in let choices = List.map (enlarge_child) (children n)
  in let compare_choices c1 c2 = fst c1 -. fst c2 |> ( *. ) 10.0 |> Float.to_int
  in let choices = List.sort (compare_choices) choices
  in choices |> List.hd |> snd (* TODO tie breakers *)

let rec add_aux entry node =
  match node.children with
  | `Entry _ -> (* goes one deeper than necessary*)
    let pnode = parent node
    in let entry = {entry with parent = Some pnode }
    in begin
      node_append entry pnode;
      (* when overflow, then split*)
      if pnode |> children |> List.length > max_entries then
        handle_overflow pnode
      else ()
    end
  | `Node lst -> begin
      node.mbr <- enlargement_rect node.mbr entry.mbr;
      add_aux entry (choose_subtree entry node)
    end

let add p x tree =
  let entry = {
    parent = None;
    mbr = Rect.of_point p;
    children = `Entry x
  }
  in add_aux entry tree


let choose_container p node =
  List.filter (fun c -> Rect.is_in p c.mbr) (children node)

(** [find_aux entry node] begins at root [node] and searches for Entry that
    matches [entry] *)
let rec find_aux p x = function
  | node :: t -> begin
      match node.children with
      | `Node lst -> find_aux p x (choose_container p node)
      | `Entry e -> if (node.mbr = (Rect.of_point p) && x = e) then
                       true, node
                    else find_aux p x t
    end
  | [] -> failwith "Can't find the node"


let find p x tree =
  (* entry is node to be find *)
  let entry = {
    parent = None;
    mbr = Rect.of_point p;
    children = `Entry x
  }
  in try find_aux p x [tree] with
  | exc -> false, entry


let rec propagate_mbr node = 
  let parent_node = node.parent in
  (** TODO, reevaluate when supporting collapsing *)
  if List.length (mbr_of_children node) = 0 then ()
  else node.mbr <- Rect.mbr_of_list (mbr_of_children node);
  match parent_node with
  | None -> ()
  | Some p -> propagate_mbr p

(* remove does not currently support collapsing--removing level case *)
let remove p x tree = 
  let found, node = find p x tree in 
  match found with 
  | true -> begin
      let parent_node = parent node in
      parent_node.children <- `Node (node_remove parent_node node);
      propagate_mbr parent_node;
    end
  | false -> ()

let union t1 t2 = empty ()

let inter t1 t2 = empty ()

let rec json_of_t t = Yojson.Basic.(
    `Assoc [
      ("bottom-left", (t.mbr |> Rect.ll |> Point.to_json));
      ("upper-right", (t.mbr |> Rect.ur |> Point.to_json));
      ("children", 
       match t.children with
       | `Node lst -> `List (List.map json_of_t lst)
       | `Entry e -> `Null
      )
    ]
  )

let to_json tree = Yojson.Basic.(`Assoc [("rtree", json_of_t tree)])

let rec mem_aux p x = function
  | node :: t -> begin
      match node.children with
      | `Node lst -> mem_aux p x (choose_container p node)
      | `Entry e -> x = e || mem_aux p x t
    end
  | [] -> false

let mem p x r = mem_aux p x [r]

open Yojson.Basic.Util

(** [unwrap_str_list lst] is the string list of tags of the given Yojson string
    list [lst] of tags. *)
let unwrap_str_list lst = 
  List.map (fun x -> to_string x) lst

(** [from_entity e] is a representation of a single data point. *)
let from_entity 
    (e : Yojson.Basic.t) : (Point.t * string list) =
  let lat = e |> member "latitude" |> to_float in
  let long = e|> member "longitude" |> to_float in
  let tags = e |> member "tags" |> to_list |> unwrap_str_list in
  ((lat, long), tags)

(** [from_json f] is the data read from a JSON file [f], containing the 
    coordinates and the tags of the points stored in the JSON.
    Required: [f] is a name of a valid json representation of data. *)
let from_json f  =
  let data_list = 
    f 
    |> Yojson.Basic.from_file 
    |> to_list
    |> List.map from_entity in 
  match data_list with
  | [] -> failwith "No data in target JSON"
  | x::[] -> new_tree (fst x) (snd x)
  | x::xs -> begin 
    let res = new_tree (fst x) (snd x) in 
    List.iter (fun x -> add (fst x) (snd x) res) xs;
    res
    end




open Rect

let max_boxes = 8
let max_entries = 8

(*
  Type requirements:
  * access parent
  * mutable bounding box for modifications
*)
type t = {
  mutable parent: t option;
  mutable mbr: Rect.t;
  mutable children: [`Node of t list | `Entry of Entry.t]
}

let empty () = {
  parent = None;
  mbr = Rect.empty;
  children = `Node [];
}

let new_tree e = 
  let root = {
    parent = None;
    mbr = Entry.mbr e;
    children = `Node [];
  } in root.children <- `Node( {
      parent = Some root;
      mbr = Entry.mbr e;
      children = `Entry e
    } :: []);
  root

let parent (n : t) : t =
  match n.parent with
  | Some m -> m
  | None -> n

let children (n : t) : t list =
  match n.children with
  | `Node lst -> lst
  | `Entry _ -> failwith "Entry does not have children"

let value (n : t) : Entry.t =
  match n.children with
  | `Node _ -> failwith "Node does not have a value"
  | `Entry e -> e

(* TODO remove;  this is for remove collapsing debugging
    removed_nodes -- number of nodes has been removed
    added_nodes   -- number of nodes has been added *)
let removed_nodes = ref 0
let added_nodes   = ref 0

(* Print added_nodes, removed_nodes *)
let print_counters uniq_nodes = Printf.printf 
    "\nadded: %d, removed: %d, size: %d\n" 
    !added_nodes !removed_nodes uniq_nodes

let reset_counters() = removed_nodes := 0; added_nodes := 0

let node_append box n =
  added_nodes := !added_nodes + 1;          (* TODO Remove *)
  match n.children with 
  | `Node lst -> n.children <- `Node (box :: lst)
  | `Entry _ -> failwith "Cannot append to an Entry"

(**[node_remove node entry] removes an [entry] from a [node]. *)
let node_remove node entry =
  removed_nodes := !removed_nodes + 1;          (* TODO Remove *)
  List.filter (fun el -> el != entry) (children node)

let mbr_of_children (n : t) : Rect.t list =
  List.map (fun c -> c.mbr) (children n)

let compare_mbr_x (compare_x : bool) (x1: t) (x2 : t) : int = 
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

let sort_subtree_x (lst: t list) (sort_x: bool) : t list = 
  List.sort (compare_mbr_x sort_x) lst

let ceil_int n  = n |> ceil |> int_of_float

(* [split n] is the result of splitting [n]  *) 
let split (n : t list) : (t list * t list) =
  let m = float_of_int (List.length n) in
  let sorted_lsts = [sort_subtree_x n true; sort_subtree_x n false] in
  let start_idx = ceil_int (0.25 *. float_of_int max_boxes) in
  let end_idx = ceil_int (m -. 0.25 *. float_of_int max_boxes) in
  let min_perimeter_sum = ref max_float in
  let min_split = ref (n, n) in
  for j = 0 to 1 do
    for i = start_idx to end_idx do
      let s, s' = split_at i (List.nth sorted_lsts j) in 
      let mbr_s = s 
                  |> List.map (fun x -> x.mbr) 
                  |> Rect.mbr_of_list in 
      let mbr_s' = s' 
                   |> List.map (fun x -> x.mbr) 
                   |> Rect.mbr_of_list in
      let new_perimeter_sum = 
        (Rect.perimeter mbr_s) +. (Rect.perimeter mbr_s') 
      in
      if new_perimeter_sum < !min_perimeter_sum 
      then begin
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
  added_nodes := !added_nodes + 1; (* TODO Remove *)
  List.iter (fun c -> c.parent <- Some node) c_lst;
  node.children <- `Node c_lst;
  node

(* [handle_overflow n] splits [n] into two bounding boxes n and n', 
   updating the children of n and its MBR, and adding n' to their
    shared parent.*)
let rec handle_overflow (n : t) : unit =
  (* split the children of n to be n and n'*)
  let u, u' = n 
              |> children 
              |> split in
  if n.parent = None then
    (* update children of n to be first result of split *)
    let n' = node_with_children n u in
    (* create new node n' around second result of split *)
    let n'' = node_with_children n u' in
    (* update bounding box of n'' *)
    n''.mbr <- n'' 
               |> mbr_of_children 
               |> Rect.mbr_of_list;
    (* update bounding box of n' *)
    n'.mbr <- n' 
              |> mbr_of_children 
              |> Rect.mbr_of_list; (* TODO factor out *)
    (* add new node to parent *)
    n.children <- `Node (n' :: n'' :: []);
    (* update bounding box of parent*)
    n.mbr <- n 
             |> mbr_of_children 
             |> Rect.mbr_of_list;
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

(* [choose_subtree e n] is the child of [n] that 
   requires the minimum increase in perimeter to include [e]. 
   The minimum increase in area is the tie-breaker.*)
let choose_subtree (e : t) (n : t) : t =
  let enlarge_child c =
    Rect.(c.mbr |> enlargement_rect e.mbr |> area, c)
  in let choices = List.map (enlarge_child) (children n)
  in let compare_choices c1 c2 = 
       fst c1 -. fst c2 
       |> ( *. ) 10.0 
       |> Float.to_int
  in let choices = List.sort (compare_choices) choices
  in choices 
     |> List.hd 
     |> snd (* TODO tie breakers *)

let rec add_aux entry node =
  match node.children with
  | `Entry _ -> (* goes one deeper than necessary*)
    let pnode = parent node
    in 
    let entry = {entry with parent = Some pnode }
    in begin
      node_append entry pnode;
      (* when overflow, then split*)
      if pnode 
         |> children 
         |> List.length > max_entries then
        handle_overflow pnode
      else ()
    end
  | `Node [] ->
    let entry = {entry with parent = Some node }
    in node_append entry node;
  | `Node _ -> begin
      node.mbr <- enlargement_rect node.mbr entry.mbr;
      add_aux entry (choose_subtree entry node)
    end

let add e tree =
  let entry = {
    parent = None;
    mbr = Entry.mbr e;
    children = `Entry e
  }
  in add_aux entry tree

let choose_container p node =
  List.filter (fun c -> Rect.is_in p c.mbr) (children node)

(** [find_aux entry node] begins at root [node] and searches for Entry that
    matches [entry] *)
let rec find_aux ent = function
  | node :: t -> begin
      match node.children with
      | `Node _ -> find_aux ent 
                     (choose_container (Entry.loc ent) node)
      | `Entry e -> 
        if (node.mbr = (Entry.mbr ent) && Entry.id ent = Entry.id e) then
          begin
            true, node
          end
        else 
          begin
            find_aux ent t
          end
    end
  | [] -> failwith "Can't find the node"

let find e tree =
  (* entry is node to be find *)
  let entry = {
    parent = None;
    mbr = Entry.mbr e;
    children = `Entry e
  }
  in try find_aux e [tree] with
  | _ -> false, entry

let rec search c r node =
  let acc = ref [] in
  if not (Circle.intersect c r node.mbr) 
  then [] 
  else
    match node.children with
    | `Entry x -> [x]
    | `Node lst -> begin
        List.iter (fun el -> acc := !acc @ search c r el) lst;
        !acc
      end

let rec propagate_mbr node =
  match node with
  | None -> ()
  | Some me ->
    me.mbr <- Rect.mbr_of_list (mbr_of_children me);
    propagate_mbr me.parent

let rec remove_collapse node child =
  match node with
  | None -> ()
  | Some me ->
    me.children <- `Node (node_remove me child);
    if List.length (children me) = 0 then
      begin
        me.mbr <- Rect.empty;        
        (* only necessary for root, but it does not hurt *)
        remove_collapse me.parent me;
      end
    else
      begin
        me.mbr <- Rect.mbr_of_list (mbr_of_children me);
        propagate_mbr me.parent;
      end

let remove e tree =
  let found, node = find e tree in
  match found with
  | true -> remove_collapse node.parent node
  | false -> ()

let rec to_list node =
  match node.children with
  | `Node lst -> List.fold_left (
      fun acc el -> acc @ (to_list el)) [] lst
  | `Entry e ->  [e]

let rec length node =
  match node.children with
  | `Node lst -> 1 + List.fold_right 
                   (fun el t -> max (length el) t) lst 0
  | `Entry _ -> 1

(* FIXME *)
let rec json_of_t t =
  `Assoc [
    ("bottom-left", (t.mbr |> Rect.ll |> Point.to_json));
    ("upper-right", (t.mbr |> Rect.ur |> Point.to_json));
    ("children", 
     match t.children with
     | `Node lst -> `List (List.map json_of_t lst)
     | `Entry e -> Entry.to_json e
    )
  ]

let to_json tree = `Assoc [("rtree", json_of_t tree)]

let rec mem_aux x = function
  | node :: t -> begin
      match node.children with
      | `Node _ -> mem_aux x (choose_container (Entry.loc x) node)
      | `Entry e -> (Entry.id x = Entry.id e) || mem_aux x t
    end
  | [] -> false

let mem x r = mem_aux x [r]
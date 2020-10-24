open Point
open Rect

let max_boxes = 8
let max_entries = 8

(*
  Type requirements:
  * access parent
  * mutable bounding box for modifications
*)


(* type ('a, 'b) leaf_or_node = 
   | Leaf of (Point.t * 'b) list  
   | Node of 'a list *)

type 'a t = {
  parent: 'a t option;
  mutable mbr: Rect.t;
  mutable children: [`Node of 'a t list | `Entry of 'a]
}


(* let empty_leaf emp par = {
   parent = Some par;
   mbr = Rect.empty;
   children = `Entry (Some emp);
   } *)

(* let empty emp = 
   let root = {
    parent = None;
    mbr = Rect.empty;
    children = `Node [];
   } in root.children <- `Node ( (empty_leaf emp root) :: []);
   root *)

let empty_leaf par = {
  parent = Some par;
  mbr = Rect.empty;
  children = `Entry None;
}

let empty emp = 
  {
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

let mbr_of_children (n : 'a t) : Rect.t list =
  List.map (fun c -> c.mbr) (children n)

(* let leaf_append (Leaf leaf) (entry) = 
   Leaf (entry :: leaf) *)

(* let node_append (Node node) (box) =
   Node (box :: node) *)

(* let lon_append lon x =
   match lon with
   | Node _ -> node_append lon x
   | Leaf _ -> leaf_append lon x *)

(* 
type 'a t =
  | Node of (Rect.t * 'a t) option array
  | Leaf of (Point.t * 'a) option array


let empty_node () = Array.make max_boxes None
let empty () = Node (empty_node ())

let unwrap_option = function
  | None -> failwith "No element to unwrap."
  | Some a -> a *)

(* * [size t] is the
   let rec size = function
   | Node boxes -> 0
   | Leaf entries -> 0 *)

(*
let split_leaf u
	m = the number of points in u
  repeat following for y-dim
    sort the points of u on x-dimension
      for i = ciel 0.4B to m − ciel 0.4B
      S1 = the set of the first i points in the list
      S2 = the set of the other i points in the list
      calculate the perimeter sum of MBR(S1) and MBR(S2); record it
        if this is the best split so far
	return the best split found

let split_node u =
	m = the number of points in u
  repeat following for right boundaries on x-dim and w.r.t to y-dim
    sort the rectangles in u by their left boundaries on the x-dimension
    for i = ciel 0.4B to m − ciel 0.4B
      S1 = the set of the first i rectangles in the list
      S2 = the set of the other i rectangles in the list
      calculate the perimeter sum of MBR(S1) and MBR(S2); record it
        if this is the best split so far
  return the best split found

let split u = 
	if u is a node then split_node u
  else split_leaf u

let handle_overflow u = 
	let u, u' = split u
  if u = root then
  	create new root;
    add u and u' as child nodes;
  else
  	let w = parent u
    update MBR of u in w
    add 'u as child of w
    if w overflows then
    	handle-overflow w

let choose_subtree u p =
	return child whose MBR requires minimum increase in perimeter to cover p
  tie-break with area

add
  if length of u can accomodate p
    p :: u
  else 

  if u overflows then
  	handle_overflow u
  else
  	let v = choose_subtree u p
    insert p v
 *)

let compare_mbr_x (compare_x : bool) (x1: 'a t) (x2 : 'a t) : int = 
  let p1 = fst x1.mbr in 
  let p2 = fst x2.mbr in
  let c1 = if compare_x then fst p1 else snd p1 in
  let c2 = if compare_x then fst p2 else snd p2 in
  Stdlib.compare c1 c2

let compare_mbr (obj1: 'a t) (obj2: 'a t) : int =
  Stdlib.compare (area obj1.mbr) (area obj2.mbr)

let split_at (n : int) (lst : 'a list) : ('a list * 'a list) = 
  let rec helper n lst acc = 
    if n = 0 then ([], lst) else
      match lst with
      | [] -> (acc, [])
      | h :: t -> helper (n - 1) t (h::acc) in
  helper n lst []

let sort_subtree (lst: 'a t list) = 
  List.sort compare_mbr lst

let sort_subtree_x (lst: 'a t list) (sort_x: bool) : 'a t list = 
  List.sort (compare_mbr_x sort_x) lst

let ceil_int n  = n |> ceil |> int_of_float

(* let perimeter_sum (n : 'a t list) : float = 
   List.fold_left (fun acc x -> acc +. (Rect.perimeter x.mbr)) 0. n  *)

(* Perhaps would be easier to split arrays rather than nodes? *)
(* [split n] is the result of splitting [n],  *) 
let split (n : 'a t list) : ('a t list * 'a t list) =
  let m = float_of_int (List.length n) in
  let sorted_by = [sort_subtree_x n true; sort_subtree_x n false] in
  (* let sorted_y = sort_subtree n false in *)
  let start_idx = ceil_int (0.25 *. float_of_int max_boxes) in
  let end_idx = ceil_int (m -. 0.25 *. float_of_int max_boxes) in
  let min_perimeter_sum = ref max_float in
  let min_split = ref (n, n) in
  for j = 0 to 1 do
    for i = start_idx to end_idx do
      (* 
        compare sets of the first i sets and (m -i) sets for both x-wise split
        and y-wise split from start_idx to end_idx and update the min_perimeter 
        and min_split accordingly.
        *)
      let s, s' = split_at i (List.nth sorted_lsts j) in 
      let mbr_s = s |> List.map (fun x -> x.mbr) |> Rect.mbr_of_list in 
      let mbr_s' = s' |> List.map (fun x -> x.mbr) |> Rect.mbr_of_list in
      let new_perimeter_sum = (Rect.perimeter mbr_s) +. (Rect.perimeter mbr_s') in
      if new_perimeter_sum < !min_perimeter_sum then 
        begin
          min_perimeter_sum := new_perimeter_sum; 
          min_split := (s, s')
        end
    done;
  done;
  !min_split  


(* [handle_overflow n] splits [n] into two bounding boxes n and n', updating the
   children of n and its MBR, and adding n' to their shared parent.*)
let rec handle_overflow (n : ('a t)) : unit =
  (* split the children of n to be n and n'*)
  let u, u' = n |> children |> split in
  if n.parent = None then
    (* update children of n to be first result of split *)
    let n' = {
      parent = Some n;
      mbr = Rect.empty;
      children = `Node u
    } in
    (* create new node n' around second result of split *)
    let n'' = {
      parent = Some n;
      mbr = Rect.empty;
      children = `Node u';
    } in
    n''.mbr <- n'' |> mbr_of_children |> Rect.mbr_of_list;
    (* update bounding box of n *)
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
    n.mbr <- n |> mbr_of_children |> Rect.mbr_of_list; (* FIXME *)
    (* create new node n' around second result of split *)
    let n' = {
      parent = Some n;
      mbr = Rect.empty;
      children = `Node u';
    } in
    (* add new node to parent *)
    w.children <- `Node (n' :: children w);
    (* update bounding box of parent *)
    w.mbr <- w |> mbr_of_children |> Rect.mbr_of_list; (* FIXME *)
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
  in let choice = choices |> List.hd |> snd (* TODO tie breakers *)
  in choice.mbr <- enlargement_rect choice.mbr p.mbr;
  choice

let rec add_aux entry node =
  match node.children with
  | `Entry _ -> (* goes one deeper than necessary*)
    let pnode = parent node
    in let entry = {entry with parent = Some pnode }
    in begin
      node_append entry pnode;
      if pnode |> children |> List.length > max_entries then
        handle_overflow pnode
      else ()
    end
  | `Node lst -> add_aux entry (choose_subtree entry node)

let add p x tree =
  let entry = {
    parent = None;
    mbr = Rect.of_point p;
    children = `Entry x
  }
  in add_aux entry tree

let remove x t = ()

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

let choose_container p node =
  List.filter (fun c -> Rect.is_in p c.mbr) (children node)

let rec mem_aux p x = function
  | node :: t -> begin
      match node.children with
      | `Node lst -> mem_aux p x (choose_container p node)
      | `Entry e -> x = e || mem_aux p x t
    end
  | [] -> false

let mem p x r = mem_aux p x [r] 
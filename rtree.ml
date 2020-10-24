open Point
open Rect

let max_boxes = 8
let max_entries = 8

(*
  Type requirements:
  * access parent
  * mutable bounding box for modifications

  type 'a t = 
	Node of {
  	parent: 'a t;
    mutable mbr: Rect.t;
    mutable children: 'a t list
  }
  | Leaf of {
  	parent: 'a t;
    mutable mbr : Rect.t;
  	mutable entries: 'a Geopoint.t list
    }
  | Root
*)


type ('a, 'b) leaf_or_node = 
  | Leaf of (Point.t * 'b) list  
  | Node of 'a list

(* I think we should separate children into two fields, one with the record and
   one with the list *)
type 'a t = {
  parent: 'a t option;
  mutable mbr: Rect.t;
  mutable children: [`Node of 'a t list | `Entry of 'a option]
}


let empty_leaf () = {
  parent = None;
  mbr = Rect.empty;
  children = `Entry None;
}

let empty () = {
  parent = None;
  mbr = Rect.empty;
  children = `Node [empty_leaf ()];
}

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
  | `Entry Some e -> e
  | `Entry None -> failwith "Tried to access the anchor"

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


insert (p, u)
	if u is leaf then
  	add p to u
  if u overflows then
  	handle_overflow u
  else
  	let v = choose_subtree u p
    insert p v
 *)

let compare_mbr (compare_x : bool) (x1: 'a t) (x2 : 'a t) : int = 
  let p1 = fst x1.mbr in 
  let p2 = fst x1.mbr in
  let c1 = if compare_x then fst p1 else snd p1 in
  let c2 = if compare_x then fst p2 else snd p2 in
  Stdlib.compare c1 c2


let sort_subtree 
    (lst: 'a t list)
    (sort_x: bool) : 'a t list = 
  List.sort (compare_mbr sort_x) lst

(* Perhaps would be easier to split arrays rather than nodes? *)
(* [split n] is the result of splitting [n],  *) 
let split (n : 'a t list) : ('a t list * 'a t list) =
  let m = List.length n in
  let sorted_x = sort_subtree n true in
  let sorted_y = sort_subtree n false in
  (*
    TODO
  *)
  failwith "unimplemented" 



(* [handle_overflow n] splits [n] into two bounding boxes n and n', updating the
    children of n and its MBR, and adding n' to their shared parent.*)
let rec handle_overflow (n : ('a t)) : unit =
  (* split the children of n to be n and n'*)
  let u, u' = n |> children |> split in
  if n.parent = None then
    (* update children of n to be first result of split *)
    let n' = {
      parent = Some n;
      mbr = Rect.empty; (* FIXME *)
      children = `Node u
    } in
    (* create new node n' around second result of split *)
    let n'' = {
      parent = Some n;
      mbr = Rect.empty; (* FIXME *)
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
    Rect.(c.mbr |> enlargement_rect p.mbr |> perimeter, c)
  in let choices = List.map (enlarge_child) (n |> children)
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
      if pnode |> children |> List.length > max_entries then
        handle_overflow pnode
      else ()
    end
  | `Node lst -> add_aux entry (choose_subtree entry node)

let add p x tree =
  let entry = {
    parent = None;
    mbr = Rect.of_point p;
    children = `Entry (Some x)
  }
  in add_aux entry tree

let remove x t = ()

let union t1 t2 = empty ()

let inter t1 t2 = empty ()


open Rtree

let rec ints_from_to lb ub =
  match lb = ub with
  | true -> []
  | false -> lb :: ints_from_to (lb + 1) ub

let entries_of_int_range lst =
  List.map (fun i -> ((float_of_int i, float_of_int i), i)) lst

let floats_from_to lb ub =
  let lst = ints_from_to lb ub in
  entries_of_int_range lst

let add_from_to lb ub t =
  if lb < ub then let lst = floats_from_to lb ub in
    List.iter (fun (p, x) -> add p x t) lst
  else let lst = List.rev (floats_from_to ub lb) in
    List.iter (fun (p, x) -> add p x t) lst

(** [add_rand base range n t] inserts  to tree [t] 
    [n] random entries between [base] and [base] + [range]*)
let add_rand base range n t =
  Random.self_init ();
  for i = 1 to n + 1 do
    let x_int = base + (Random.int range) in
    let x_float = float_of_int x_int in
    add (x_float, x_float) x_int t
  done

let add_random range n t = add_rand 0 range n t

let add_cluster mid rad n t = add_rand (mid - rad) (rad * 2) n t
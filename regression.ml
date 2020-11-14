open Rtree

let rec ints_from_to lb ub =
  match lb = ub with
  | true -> []
  | false -> lb::ints_from_to (lb + 1) ub

let entries_of_int_range lst =
  List.map (fun i -> ((float_of_int i, float_of_int i), i)) lst

let rec floats_from_to lb ub =
  let rec floats_from_to_aux lb ub acc =
    match lb = ub with
    | true -> acc
    | false -> floats_from_to_aux (lb + 1) ub 
                 (((float_of_int lb, float_of_int lb), string_of_int lb)::acc)
  in List.rev (floats_from_to_aux lb ub [])

let add_from_to lb ub t =
  if lb < ub then let lst = floats_from_to lb ub in
    List.iter (fun (p, x) -> add (Entry.manual x p [] `Null) t) lst
  else let lst = List.rev (floats_from_to ub lb) in
    List.iter (fun (p, x) -> add (Entry.manual x p [] `Null) t) lst

(** [add_rand base range n t] inserts  to tree [t] 
    [n] random entries between [base] and [base] + [range]*)
let add_rand base range n t =
  Random.self_init ();
  for i = 1 to n + 1 do
    let x_int = base + (Random.int range) in
    let x_float = float_of_int x_int in
    let x_str = string_of_int x_int in
    add (Entry.manual x_str (x_float, x_float) [] `Null) t
  done

let add_random range n t = add_rand 0 range n t

let add_cluster mid rad n t = add_rand (mid - rad) (rad * 2) n t

module Log = Dolog.Log

let enable_log_info =
  begin
    Log.set_log_level Log.INFO;
    Log.color_on ();
    Log.set_output stdout;
  end

let execute (u:unit->unit) (m:string) : unit =
  begin
    let s = Unix.gettimeofday () in
    u ();
    let e = Unix.gettimeofday () in
    let elapsed = (e -. s)*.(10.**6.) in
    Log.info "%s in %.2f us" m elapsed
  end
open Lib
open OUnit2
open Rtree
open Regression

let ldist_test
    (name : string)
    (point : Point.t)
    (rect : Rect.t)
    (expected_output : float) : test =
  name >:: (fun _ ->
      assert_equal expected_output (ldist point rect)
        ~printer:string_of_float)

let entry_debug = Entry.manual "DEBUG" (0.,0.) [] `Null

let nn_test
    (name : string)
    (r : int)
    (query : Entry.t)
    (node : Rtree.t)
    (expected_output : Entry.t) =
  name >:: (fun _ ->
      let ep = match rnn r query node with
        | ([es],_) -> es
        | _ -> entry_debug in
      assert_equal expected_output ep
        ~printer:(Entry.to_string false))

let rect1 = ((-1., -1.), (1., 1.))

let entry_1 = Entry.manual "1" (1.,1.) [] `Null
let entry_2 = Entry.manual "2" (1.,2.) [] `Null
let entry_500k = Entry.manual "500000" (500000.0,500000.0) [] `Null
let entry_500kp1 = Entry.manual "500kp1" (500000.1,500000.1) [] `Null
    
let int_tree_1 = empty ()
let () = add entry_1 int_tree_1

let int_tree_2 = empty ()
let () = add entry_1 int_tree_2
let () = add entry_2 int_tree_2

let int_tree_3 = empty ()
let () = add entry_1 int_tree_3
let () = add entry_2 int_tree_3
let () = ints_from_to 3 1000000 |>
         List.rev_map
           (fun x -> (string_of_int x,
                      (float_of_int x, float_of_int x))) |>
         List.rev_map (fun (s, p) -> Entry.manual s p [] `Null) |>
         List.iter (fun e -> add e int_tree_3)
let () = add entry_500kp1 int_tree_3
           
let rnn_tests = [
  (*
  ldist_test "Left Side" (-5.56, 0.223) rect1 4.56;
  ldist_test "Right Side" (7.62, 0.308) rect1 6.62;
  ldist_test "Top Side" (0.308, 7.62) rect1 6.62;
  ldist_test "Bottom Side" (0.223,-5.56) rect1 4.56;
  ldist_test "Upper Left" (-4., 5.) rect1 5.;
  ldist_test "Upper Right" (4., 5.) rect1 5.;
  ldist_test "Bottom Left" (-4., -5.) rect1 5.;
  ldist_test "Bottom Right" (4., -5.) rect1 5.;
  ldist_test "Inside" (0.123,-0.456) rect1 0.;

  nn_test "One element" 1 entry_1 int_tree_1 entry_debug;
  nn_test "Two elements" 1 entry_1 int_tree_2 entry_2;
*)
  
  nn_test "1000000 elements" 1 entry_500k int_tree_3 entry_500kp1;
]

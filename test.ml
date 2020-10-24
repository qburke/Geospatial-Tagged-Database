open OUnit2
open Point
open Rect

let origin_pt = Point.origin
let empty_rect = Rect.empty
let pt1 = (1., 2.)
let pt2 = (2., 3.)
let pt3 = (1., 8.)
let pt4 = (0., 10.)
let rect1 = (pt1, pt2)
let rect2 = (pt1, pt3)
let rect3 = (pt2, pt4)

let tuple_printer = function
  | ((a, b), (c, d)), e -> 
    "((" ^ string_of_float a ^ ", " ^ string_of_float b ^ "), (" 
    ^ string_of_float c ^ ", " ^ string_of_float d ^ ")), " ^ 
    string_of_float e ^ ")"

let rect_printer = function
  | ((a, b), (c, d)) -> 
    "((" ^ string_of_float a ^ ", " ^ string_of_float b ^ "), (" 
    ^ string_of_float c ^ ", " ^ string_of_float d ^ "))"

(*let enlargement_pt_test 
    (name : string) 
    (pt : Point.t) 
    (rect : Rect.t)
    (expected_output : Rect.t * float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (enlargement_pt pt rect) 
        ~printer:tuple_printer)*)

let enlargement_rect_test 
    (name : string) 
    (rect : Rect.t) 
    (rect' : Rect.t)
    (expected_output : Rect.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (enlargement_rect rect rect') 
        ~printer:rect_printer)

let rect_tests = [
  (*enlargement_pt_test "empty" origin_pt empty_rect (empty_rect, 0.);
    enlargement_pt_test "origin pt" origin_pt rect1 (((0., 0.), (2., 3.)), 4.);
    enlargement_pt_test "pt1 rect1" pt1 rect1 (rect1, 0.);
    enlargement_pt_test "pt3 rect1" pt3 rect1 (((1., 2.), (2., 8.)), 5.);*)
  enlargement_rect_test "rect1 rect2" rect1 rect2 ((1., 2.), (2., 8.))
]

open Rtree
let int_tree_1 = new_tree (0.,0.) 0
let () = add (1., 2.) 3 int_tree_1
let () = add (4., 2.) 3110 int_tree_1
let () = add (1.1, 2.1) 5 int_tree_1
let () = add (0.9, 1.9) 64 int_tree_1

let int_tree_2 = new_tree (0., 0.) 0
let () = for i=0 to 10 do
    add (float_of_int i, float_of_int i) (i) int_tree_2
  done

let mem_test
    (name : string)
    (loc : Point.t)
    (data : 'a)
    (tree : 'a t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (mem loc data tree) ~printer:string_of_bool)

let out_json_test
    (name : string)
    (filename : string)
    (tree : 'a t) : test =
  name >:: (fun _ ->
      assert_equal () (tree |> to_json |> Yojson.Basic.to_file filename))

let rtree_tests = [
  mem_test "3 at (1., 2.) in int_tree_1" (1., 2.) 3 int_tree_1 true;
  mem_test "3110 at (4., 2.) in int_tree_1" (4., 2.) 3110 int_tree_1 true;
  mem_test "5 at (1.1, 2.1) in int_tree_1" (1.1, 2.1) 5 int_tree_1 true;
  mem_test "64 at (0.9, 1.9) in int_tree_1" (0.9, 1.9) 64 int_tree_1 true;
  mem_test "5 not at (1., 2.) in int_tree_1" (1., 2.) 5 int_tree_1 false;
  mem_test "64 not at (1., 2.) in int_tree_1" (1., 2.) 64 int_tree_1 false;
]

let suite =
  "test suite for final_project"  >::: List.flatten [
    rect_tests;
    rtree_tests;
  ]

let _ = run_test_tt_main suite
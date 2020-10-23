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


let rtree_tests = [

]

let suite =
  "test suite for final_project"  >::: List.flatten [
    rect_tests;
    rtree_tests;
  ]

let _ = run_test_tt_main suite
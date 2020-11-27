open Lib
open OUnit2
open Rect
open Regression

let origin_pt = Point.origin
let empty_rect = Rect.empty
let pt1 = (1., 2.)
let pt2 = (2., 3.)
let pt3 = (1., 8.)
let pt4 = (0., 10.)
let rect1 = (pt1, pt2)
let rect2 = (pt1, pt3)
let rect3 = (pt2, pt4)

let rect_printer = function
  | ((a, b), (c, d)) -> 
    "((" ^ string_of_float a ^ ", " ^ string_of_float b ^ "), (" 
    ^ string_of_float c ^ ", " ^ string_of_float d ^ "))"

let enlargement_rect_test 
    (name : string) 
    (rect : Rect.t) 
    (rect' : Rect.t)
    (expected_output : Rect.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (enlargement_rect rect rect') 
        ~printer:rect_printer)

let mbr_of_list_test
    (name : string)
    (lst : Rect.t list)
    (expected_output: Rect.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (mbr_of_list lst) ~printer:rect_printer)

let points_1 = List.map
    (fun i -> Rect.of_point (float_of_int i, float_of_int i)) 
    (ints_from_to 0 10)

let rect_tests = [
  enlargement_rect_test "rect1 rect2" rect1 rect2 ((1., 2.), (2., 8.));
  enlargement_rect_test "rect1 rect2" Rect.empty ((3., 3.), (3., 3.)) 
    ((0., 0.), (3., 3.));
  mbr_of_list_test "MBR of points (0,0) to (9,9) is (0,0), (9,9)" points_1 
    ((0., 0.), (9., 9.))
]

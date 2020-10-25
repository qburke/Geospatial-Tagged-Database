open OUnit2
open Point
open Rect
open Db

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


let init_obj = create_element "nil" ["nil"]

let make_db (elems : 'a element list) =
  let db = create_db init_obj in
  ignore(List.map
    (fun elem -> add db elem) elems);
  db

let list_of_tags_test
    (name : string)
    (db : 'a database)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal
        (List.sort compare expected_output)
        (list_of_reverse_index  db))
    
let tag_contents_test
    (name : string)
    (db : 'a database)
    (tag : string)
    (expected_output : string list) : test =
  name >::  (fun _ ->
      assert_equal
        (List.sort compare expected_output)
        (list_of_tag_collection db tag |>
         List.map data_of_element |> List.sort compare))

let tag_search_test
    (name : string)
    (db : 'a database)
    (objects : 'a element list)
    (tags : string list)
    (expected_output : string list) =
  name >:: (fun _ ->
      assert_equal
        (List.sort compare expected_output)
        (tag_search db objects tags |>
         List.map data_of_element |>
         List.sort compare))

let obj1 = create_element "obj1" ["tag1"]
let obj2 = create_element "obj2" ["tag2"]
let obj3 = create_element "obj3" ["tag2"]
let obj4 = create_element "obj4" ["tag1"; "tag2"; "tag3"]
let empty_db = make_db []
let one_tag_db = make_db [obj1]
let two_tag_db = make_db [obj1; obj2]
let multi_tag_element_db = make_db [obj1; obj2; obj3; obj4]  

let db_tests =  [
  list_of_tags_test "Contains no tags"
    (make_db []) [];
  list_of_tags_test "Contains one tag"
    one_tag_db ["tag1"];
  list_of_tags_test "Contains two tags"
    two_tag_db
    ["tag1"; "tag2"];
  list_of_tags_test "Element in multiple tags"
    multi_tag_element_db
    ["tag1"; "tag2"; "tag3"];
  tag_contents_test "Tag with one element"
    one_tag_db "tag1" ["obj1"];
  tag_contents_test "Tag with multiple elements"
    multi_tag_element_db "tag2"
    ["obj2";"obj3";"obj4"];
  tag_search_test "One element one tag query"
    multi_tag_element_db [obj1] ["tag1"] ["obj1"];
  tag_search_test "Two element one tag query no 1"
    multi_tag_element_db [obj1; obj2] ["tag1"] ["obj1"];
  tag_search_test "Two element one tag query no 2"
    multi_tag_element_db [obj1; obj2] ["tag2"] ["obj2"];
  tag_search_test "One element two tag query"
    multi_tag_element_db [obj1] ["tag1"; "tag2"] [];
  tag_search_test "Multi-element n-tag query 01"
    multi_tag_element_db [obj1; obj2; obj3; obj4] ["tag1"] ["obj1"; "obj4"];
  tag_search_test "Multi-element n-tag query 02"
    multi_tag_element_db [obj1; obj2; obj3; obj4]
    ["tag1"; "tag2"; "tag3"] ["obj4"];
  tag_search_test "Multi-element n-tag query 03"
    multi_tag_element_db [obj1; obj2; obj3; obj4]
    ["tag2"] ["obj2";"obj3"; "obj4"]
]


let rect_tests = [
  (*enlargement_pt_test "empty" origin_pt empty_rect (empty_rect, 0.);
    enlargement_pt_test "origin pt" origin_pt rect1 (((0., 0.), (2., 3.)), 4.);
    enlargement_pt_test "pt1 rect1" pt1 rect1 (rect1, 0.);
    enlargement_pt_test "pt3 rect1" pt3 rect1 (((1., 2.), (2., 8.)), 5.);*)
  enlargement_rect_test "rect1 rect2" rect1 rect2 ((1., 2.), (2., 8.))
]

open Rtree
let int_tree_1 = empty 0
let () = add (1., 2.) 3 int_tree_1
let () = add (4., 2.) 3110 int_tree_1
let () = add (1.1, 2.1) 5 int_tree_1
let () = add (0.9, 1.9) 64 int_tree_1

let mem_test
    (name : string)
    (loc : Point.t)
    (data : 'a)
    (tree : 'a t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (mem loc data tree) ~printer:string_of_bool)

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
    db_tests;
  ]

let _ = run_test_tt_main suite

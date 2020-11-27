open Lib
open OUnit2
open Db

let init_obj = create_element "nil" Point.origin ["nil"] `Null

let make_db (elems : element list) =
  let db = create_db "test db" in
  ignore(List.map
           (fun elem -> add db elem) elems);
  db

let list_of_tags_test
    (name : string)
    (db : database)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal
        (List.sort compare expected_output)
        (list_of_reverse_index  db))

let tag_contents_test
    (name : string)
    (db : database)
    (tag : string)
    (expected_output : string list) : test =
  name >::  (fun _ ->
      assert_equal
        (List.sort compare expected_output)
        (list_of_tag_collection db tag |>
         List.map id_of_element |> List.sort compare))

let tag_search_test
    (name : string)
    (db : database)
    (objects : element list)
    (tags : string list)
    (expected_output : string list) =
  name >:: (fun _ ->
      assert_equal
        (List.sort compare expected_output)
        (tag_search db objects tags |>
         List.map id_of_element |>
         List.sort compare))

let obj1 = create_element "obj1" Point.origin ["tag1"] `Null
let obj2 = create_element "obj2" Point.origin ["tag2"] `Null
let obj3 = create_element "obj3" Point.origin ["tag2"] `Null
let obj4 = create_element "obj4" Point.origin ["tag1"; "tag2"; "tag3"] `Null
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

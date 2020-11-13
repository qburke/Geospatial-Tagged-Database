open OUnit2
open Point
open Rect
open Db
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

let tuple_printer = function
  | ((a, b), (c, d)), e -> 
    "((" ^ string_of_float a ^ ", " ^ string_of_float b ^ "), (" 
    ^ string_of_float c ^ ", " ^ string_of_float d ^ ")), " ^ 
    string_of_float e ^ ")"

let rect_printer = function
  | ((a, b), (c, d)) -> 
    "((" ^ string_of_float a ^ ", " ^ string_of_float b ^ "), (" 
    ^ string_of_float c ^ ", " ^ string_of_float d ^ "))"

(** [enlargement_rect_test name r1 r2 expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [enlargement_rect r1 r2]. *)
let enlargement_rect_test 
    (name : string) 
    (rect : Rect.t) 
    (rect' : Rect.t)
    (expected_output : Rect.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (enlargement_rect rect rect') 
        ~printer:rect_printer)


let init_obj = create_element "nil" ["nil"] Point.origin

let make_db (elems : 'a element list) =
  let db = create_db "test db" init_obj in
  ignore(List.map
           (fun elem -> add db elem) elems);
  db

(** [list_of_tags_test name db expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [list_of_reverse_index db]. *)
let list_of_tags_test
    (name : string)
    (db : 'a database)
    (expected_output : string list) : test =
  name >:: (fun _ ->
      assert_equal
        (List.sort compare expected_output)
        (list_of_reverse_index  db))

(** [tag_contents_test name db tag expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [list_of_tag_collection db tag]. *)
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

(** [tag_search_test name db objects tags expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [tag_search_test db objects tags]. *)
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

let obj1 = create_element "obj1" ["tag1"] Point.origin
let obj2 = create_element "obj2" ["tag2"] Point.origin
let obj3 = create_element "obj3" ["tag2"] Point.origin
let obj4 = create_element "obj4" ["tag1"; "tag2"; "tag3"] Point.origin
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

(** [mbr_of_list_test name lst expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [mbr_of_list lst]. *)
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

open Rtree
let int_tree_1 = new_tree (0.,0.) 0
let () = add (1., 2.) 3 int_tree_1
let () = add (4., 2.) 3110 int_tree_1
let () = add (1.1, 2.1) 5 int_tree_1
let () = add (0.9, 1.9) 64 int_tree_1

let int_tree_2 = new_tree (0., 0.) 0
let int_tree_2_entries = 10 |> ints_from_to 0 |> entries_of_int_range

let int_tree_3 = new_tree (0., 0.) 0
let () = List.iter (fun (p, x) -> add p x int_tree_3) int_tree_2_entries

let int_tree_4 = new_tree (0., 0.) 0
let () = List.iter
    (fun (p, x) -> add p x int_tree_4)
    (10000 |> ints_from_to 0 |> entries_of_int_range)

(** [add_test name entries tree dir file expected_output] constructs an OUnit
    test named [name] that asserts that adding each element in [entries] to
    [tree] was successful. *)
let add_test
    (name : string)
    (entries : (Point.t * 'a) list)
    (tree : 'a t) : test list =
  List.mapi
    (fun i (p, x) ->
       name >::
       (fun _ -> begin
            add p x tree;
            assert_equal true (mem p x tree) ~printer:string_of_bool
          end)
    )
    entries

(** [remove_test name entries tree expected_output] constructs an OUnit
    test named [name] that asserts that removing each element in [entries] to
    [tree] returns [unit]. *)
let remove_test 
    (name : string)
    (entries : (Point.t * 'a) list)
    (tree : 'a t) : test list =
  List.map
    (fun (p, x) ->
       name >:: (fun _ ->
           assert_equal () (remove p x tree))
    )
    entries

let find_test 
    (name : string)
    (entries : (Point.t * 'a) list)
    (tree : 'a t)
    (output : bool * 'a Rtree.t) : test list =
  List.map
    (fun (p, x) ->
       name >:: (fun _ ->
           assert_equal output (find p x tree))
    )
    entries

(** [mem_test name p x tree expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [mem p x tree]. *)
let mem_test
    (name : string)
    (loc : Point.t)
    (data : 'a)
    (tree : 'a t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (mem loc data tree) ~printer:string_of_bool)

(** [mem_test name p x tree expected_output] constructs an OUnit
    test named [name] that asserts each entry is a member of [tree]. *)
let mem_list_test
    (name : string)
    (entries : (Point.t * 'a) list)
    (tree : 'a t) : test list =
  List.map
    (fun (p, x) ->
       name >:: (fun _ ->
           assert_equal true (mem p x tree) ~printer:string_of_bool)
    )
    entries

(** [out_json_test name filname tree] constructs an OUnit test named [name] that
    outputs the JSON representation of [tree] to [filename]. *)
let out_json_test
    (name : string)
    (filename : string)
    (tree : 'a t) : test =
  name >:: (fun _ ->
      assert_equal () (tree |> to_json |> Yojson.Basic.to_file filename))

let rtree_tests = List.flatten [
    [
      mem_test "3 at (1., 2.) in int_tree_1" (1., 2.) 3 int_tree_1 true;
      mem_test "3110 at (4., 2.) in int_tree_1" (4., 2.) 3110 int_tree_1 true;
      mem_test "5 at (1.1, 2.1) in int_tree_1" (1.1, 2.1) 5 int_tree_1 true;
      mem_test "64 at (0.9, 1.9) in int_tree_1" (0.9, 1.9) 64 int_tree_1 true;
      mem_test "5 not at (1., 2.) in int_tree_1" (1., 2.) 5 int_tree_1 false;
      mem_test "64 not at (1., 2.) in int_tree_1" (1., 2.) 64 int_tree_1 false;
    ];
    [
      (*remove_test "3 at (1., 2.) in int_tree_1" (1., 2.) 3 int_tree_1;*)

    ];
    add_test "Add 10 records to int_tree_2" int_tree_2_entries int_tree_2;
    mem_list_test "Check 10 records are in int_tree_3" int_tree_2_entries 
      int_tree_3;
    [
      out_json_test "int_tree_3 out to int_tree_3.json" "int_tree_3.json" 
        int_tree_3;
      out_json_test "1000 elements" "thousand.json" int_tree_4;
    ]
  ]

let suite =
  "test suite for final_project"  >::: List.flatten [
    rect_tests;
    rtree_tests;
    db_tests;
  ]

let _ = run_test_tt_main suite

let assert_find p x expected_result tree =
  let found, entry = find p x tree in
  assert(found = expected_result)

(**  Test Pass, tree without split
     let () = add (2.5, 2.5) 25 int_tree_1
     let () = assert_find (2.5, 2.5) 25 true int_tree_1
     let () = add (3.5, 3.5) 35 int_tree_1
     let () = assert_find (3.5, 3.5) 35 true int_tree_1
     let () = remove (3.5, 3.5) 35 int_tree_1
     let () = assert_find (3.5, 3.5) 35 false int_tree_1
     let () = remove (2.5, 2.5) 25 int_tree_1
     let () = assert_find (2.5, 2.5) 25 false int_tree_1
*)


(**  TODO:  Restore
     let number_entries = 101
     let int_test_tree_entries = number_entries 
     |> ints_from_to 1 |> entries_of_int_range
     let int_test_tree = new_tree (0., 0.) 0
     let () = List.iter (fun (p, x) -> 
     add p x int_test_tree) int_test_tree_entries

     let () = remove (9.,9.) 9 int_test_tree
     let () = assert_find (9.,9.) 9 false int_test_tree
     let () = remove (8.,8.) 8 int_test_tree
     let () = assert_find (8.,8.) 8 false int_test_tree
     let () = remove (7.,7.) 7 int_test_tree
     let () = assert_find (7.,7.) 7 false int_test_tree
     let () = remove (30.,30.) 30 int_test_tree
     let () = assert_find (30.,30.) 30 false int_test_tree
     let () = remove (100.,100.) 100 int_test_tree
     let () = assert_find (100.,100.) 100 false int_test_tree
*)

let elements_count = 1000000

(* [add] regression increasing order *)
let int_test_tree = new_tree (0., 0.) 0
let () = execute (fun () -> add_from_to 0 elements_count int_test_tree)
    (Printf.sprintf "test adding %d elements increasing order" elements_count)

(* [add] regression decreasing order *)
let int_test_tree = new_tree (0., 0.) 0
let () = execute (fun () -> add_from_to elements_count 0 int_test_tree)
    (Printf.sprintf "test adding %d elements decreasing order" elements_count)

(* [add] regression random order *)
let int_test_tree = new_tree (0., 0.) 0
let () = execute (fun () -> add_random elements_count 
                     elements_count int_test_tree)
    (Printf.sprintf "test adding %d random elements" elements_count)

(* [add] regression cluster element *)
let int_test_tree = new_tree (float_of_int elements_count, 
                              float_of_int elements_count) elements_count
let () = execute (fun () -> add_cluster elements_count 
                     (elements_count/10) elements_count int_test_tree)
    (Printf.sprintf "test adding %d cluster elements" elements_count)

(* [length] regression *)
let int_test_tree = new_tree (0., 0.) 0
let () = add_from_to 0 elements_count int_test_tree
let () = Printf.printf "Tree with %d element has tree length of = %d\n" 
    elements_count (length int_test_tree)


let elements_count = 100000

let int_test_tree = new_tree (0., 0.) 0
let () = add_from_to 0 elements_count int_test_tree

(* [find] regression *)
let lst = to_list int_test_tree
let () = execute (fun () -> List.iter (fun (p, v) -> 
    ignore (find p v int_test_tree)) lst)
    (Printf.sprintf "test finding %d cluster elements" elements_count)

(* [remove] regression *)
let () = execute (fun () -> List.iter (fun (p, v) -> 
    remove p v int_test_tree) lst)
    (Printf.sprintf "test removing %d cluster elements" elements_count)

let lst = to_list int_test_tree
let () = assert(List.length lst = 0)

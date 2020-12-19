open Lib
open OUnit2
open Regression
open Database_test
open Rect_test
open Rtree

let int_tree_1 = empty ()
let () = add (Entry.manual "3" (1., 2.) [] `Null) int_tree_1
let () = add (Entry.manual "3110" (4., 2.) [] `Null) int_tree_1
let () = add (Entry.manual "5" (1.1, 2.1) [] `Null) int_tree_1
let () = add (Entry.manual "64" (0.9, 1.9) [] `Null) int_tree_1

let entries_of_int_range lst =
  List.map (fun i ->
      Entry.manual
        (string_of_int i)
        (float_of_int i, float_of_int i)
        []
        `Null
    )
    lst

let int_tree_2 = empty ()
let int_tree_2_entries = 10 |> ints_from_to 0 |> entries_of_int_range

let int_tree_3 = empty ()
let () = List.iter (fun x -> add x int_tree_3) int_tree_2_entries

let int_tree_4 = empty ()
let () = List.iter
    (fun x -> add x int_tree_4)
    (10000 |> ints_from_to 0 |> entries_of_int_range)

(** [add_test name entries tree dir file expected_output] constructs an OUnit
    test named [name] that asserts that adding each element in [entries] to
    [tree] was successful. *)
let add_test
    (name : string)
    (entries : Entry.t list)
    (tree : t) : test list =
  List.map
    (fun x ->
       name >::
       (fun _ -> begin
            add x tree;
            assert_equal true (mem x tree) ~printer:string_of_bool
          end)
    )
    entries

(** [remove_test name entries tree expected_output] constructs an OUnit
    test named [name] that asserts that removing each element in [entries] to
    [tree] returns [unit]. *)
let remove_test 
    (name : string)
    (entries : Entry.t list)
    (tree : t) : test list =
  List.map
    (fun x ->
       name >:: (fun _ ->
           assert_equal () (remove x tree))
    )
    entries

let find_test 
    (name : string)
    (entries : Entry.t list)
    (tree : t)
    (output : bool * Rtree.t) : test list =
  List.map
    (fun x ->
       name >:: (fun _ ->
           assert_equal output (find x tree))
    )
    entries

(** [mem_test name p x tree expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [mem p x tree]. *)
let mem_test
    (name : string)
    (entry : Entry.t)
    (tree : t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output (mem entry tree) ~printer:string_of_bool)

(** [mem_test name p x tree expected_output] constructs an OUnit
    test named [name] that asserts each entry is a member of [tree]. *)
let mem_list_test
    (name : string)
    (entries : Entry.t list)
    (tree : t) : test list =
  List.map
    (fun x ->
       name >:: (fun _ ->
           assert_equal true (mem x tree) ~printer:string_of_bool)
    )
    entries

(** [out_json_test name filname tree] constructs an OUnit test named [name] that
    outputs the JSON representation of [tree] to [filename]. *)
let out_json_test
    (name : string)
    (filename : string)
    (tree : t) : test =
  name >:: (fun _ ->
      assert_equal () (tree |> to_json |> Yojson.Basic.to_file filename))

let rtree_tests = List.flatten [
    [
      mem_test "3 at (1., 2.) in int_tree_1"
        (Entry.manual "3" (1., 2.) [] `Null) int_tree_1 true;
      mem_test "3110 at (4., 2.) in int_tree_1"
        (Entry.manual "3110" (4., 2.) [] `Null) int_tree_1 true;
      mem_test "5 at (1.1, 2.1) in int_tree_1" 
        (Entry.manual "5" (1.1, 2.1) [] `Null)int_tree_1 true;
      mem_test "64 at (0.9, 1.9) in int_tree_1"
        (Entry.manual "64" (0.9, 1.9) [] `Null) int_tree_1 true;
      mem_test "5 not at (1., 2.) in int_tree_1"
        (Entry.manual "5" (1., 2.) [] `Null) int_tree_1 false;
      mem_test "64 not at (1., 2.) in int_tree_1"
        (Entry.manual "64" (1., 2.) [] `Null) int_tree_1 false;
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
    (*entry_tests;*)
  ]

let _ = run_test_tt_main suite

let assert_find x expected_result tree =
  let found, _ = find x tree in
  assert(found = expected_result)

let number_entries = 101
let int_test_tree_entries = number_entries |> ints_from_to 1 
                            |> entries_of_int_range
let int_test_tree = Rtree.empty ()
let () = List.iter (fun x -> add x int_test_tree) int_test_tree_entries

let () = remove
    (Entry.manual "9" (9., 9.) [] `Null) int_test_tree
let () = assert_find
    (Entry.manual "9" (9., 9.) [] `Null) false int_test_tree
let () = remove
    (Entry.manual "8" (8., 8.) [] `Null) int_test_tree
let () = assert_find
    (Entry.manual "8" (8., 8.) [] `Null) false int_test_tree
let () = remove
    (Entry.manual "7" (7., 7.) [] `Null) int_test_tree
let () = assert_find
    (Entry.manual "7" (7., 7.) [] `Null) false int_test_tree
let () = remove
    (Entry.manual "30" (30., 30.) [] `Null) int_test_tree
let () = assert_find 
    (Entry.manual "30" (30., 30.) [] `Null)false int_test_tree
let () = remove
    (Entry.manual "100" (100., 100.) [] `Null) int_test_tree
let () = assert_find 
    (Entry.manual "100" (100., 100.) [] `Null)false int_test_tree

(* Stress testing for 1 million elements add and 100k elements search/delete *)
let elements_count = 100000

(* [add] regression increasing order *)
let int_test_tree = empty ()
let () = execute (fun () -> add_from_to 0 elements_count int_test_tree)
    (Printf.sprintf "test adding %d elements increasing order" elements_count)

(* [add] regression decreasing order *)
let int_test_tree = empty ()
let () = execute (fun () -> add_from_to elements_count 0 int_test_tree)
    (Printf.sprintf "test adding %d elements decreasing order" elements_count)

(* [add] regression random order *)
let int_test_tree = empty ()
let () = execute (fun () -> add_random elements_count 
                     elements_count int_test_tree)
    (Printf.sprintf "test adding %d random elements" elements_count)

(* [add] regression cluster element *)
let int_test_tree = empty ()
let () = execute (fun () -> add_cluster elements_count 
                     (elements_count/10) elements_count int_test_tree)
    (Printf.sprintf "test adding %d cluster elements" elements_count)

(* [length] regression *)
let int_test_tree = empty ()
let () = add_from_to 0 elements_count int_test_tree
let () = Printf.printf "Tree with %d element has tree length of = %d\n" 
    elements_count (length int_test_tree)

let elements_count = 100000

let () = reset_counters()

let int_test_tree = empty ()
let () = add_from_to 0 elements_count int_test_tree

let () = print_counters elements_count

(* [find] regression *)
let lst = to_list int_test_tree
let () = execute (fun () -> List.iter (fun e -> 
    ignore (find e int_test_tree)) lst)
    (Printf.sprintf "test finding %d cluster elements" elements_count)

(* [remove] regression *)
let () = execute (fun () -> List.iter (fun e -> 
    remove e int_test_tree) lst)
    (Printf.sprintf "test removing %d cluster elements" elements_count)

let () = print_counters elements_count
let lst = to_list int_test_tree

let () = assert(List.length lst = 0)
let () = assert(int_test_tree = empty ())
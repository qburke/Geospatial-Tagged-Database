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

let knn_test
    (name : string)
    (r : int)
    (query : Entry.t)
    (node : Rtree.t)
    (expected_output : Entry.t list) =
  name >:: (fun _ ->
      let ep = match knn r query node with
        | [] -> [entry_debug]
        | es -> es in
      assert_equal expected_output ep)

let to_list_test
    (name : string)
    (queue : 'a Prio_queue.queue)
    (expected_output : 'a list) =
  name >:: (fun _ ->
      let lis = Prio_queue.to_list queue in
      let list = List.map (fun (_,x) -> x) lis in
      assert_equal expected_output list)

let extract_test
    (name : string)
    (queue : 'a Prio_queue.queue)
    (expected_output : 'a) =
  name >:: (fun _ ->
      try let top = Prio_queue.extract queue in
        match top with
        | (_,elt,_) -> assert_equal expected_output elt
      with
      | Prio_queue.Queue_is_empty -> failwith "Precondition violated")

let rect1 = ((-1., -1.), (1., 1.))

let entry_0 = Entry.manual "0" (0.,0.) [] `Null
let entry_50 = Entry.manual "50" (50.,25.34) [] `Null
let entry_1 = Entry.manual "1" (1.,1.) [] `Null
let entry_2 = Entry.manual "2" (1.,2.) [] `Null
let entry_500k = Entry.manual "500000" (500000.0,500000.0) [] `Null
let entry_500kp1 = Entry.manual "500kp1" (500000.1,500000.1) [] `Null

let int_tree_1 = empty ()
let () = add entry_1 int_tree_1

let int_tree_2 = empty ()
let () = add entry_1 int_tree_2
let () = add entry_2 int_tree_2

let int_tree_25 = empty ()
let () = add entry_1 int_tree_25
let () = add entry_2 int_tree_25
let () = add entry_500k int_tree_25

let int_tree_3 = empty ()
let () = add entry_1 int_tree_3
let () = add entry_2 int_tree_3

let () = ints_from_to 0 1 |> ignore

let () = ints_from_to 3 10000 |>
         List.rev_map
           (fun x -> (string_of_int x,
                      (float_of_int x, float_of_int x))) |>
         List.rev_map (fun (s, p) -> Entry.manual s p [] `Null) |>
         List.iter (fun e -> add e int_tree_3)
let () = add entry_500kp1 int_tree_3

let re_10 = Entry.manual "10"(5.14978927308,8.5720283739) [] `Null
let re_9 = Entry.manual "9" (8.69086653949,2.33855485136) [] `Null
let re_8 = Entry.manual "8" (7.65397083313,2.32738705109) [] `Null
let re_7 = Entry.manual "7" (6.99899986609,0.118325291041) [] `Null
let re_6 = Entry.manual "6" (5.63907297497,2.049598981) [] `Null
let re_5 = Entry.manual "5" (4.4629101228,2.25442525621) [] `Null
let re_4 = Entry.manual "4" (0.851489653826,3.90832001881) [] `Null
let re_3 = Entry.manual "3" (1.94751670357,2.28192433909) [] `Null
let re_2 = Entry.manual "2" (0.227701987768,1.98699567306) [] `Null
let re_1 = Entry.manual "1" (0.999518816636,0.0310183041281) [] `Null

let rre_20 = Entry.manual "20" (58.7718843519,43.3137042625) [] `Null
let rre_19 = Entry.manual "19" (64.1293461356,38.0428177103) [] `Null
let rre_18 = Entry.manual "18" (65.7942357932,33.9737776036) [] `Null
let rre_17 = Entry.manual "17" (65.4709472922,32.3862607021) [] `Null
let rre_16 = Entry.manual "16" (54.7671825114,40.6133091013) [] `Null
let rre_15 = Entry.manual "15" (57.4702877293,38.3474901976) [] `Null
let rre_14 = Entry.manual "14" (62.0731019559,32.4280328133) [] `Null
let rre_13 = Entry.manual "13" (54.6428571443,37.4826470564) [] `Null
let rre_12 = Entry.manual "12" (60.2265534609,31.618344074) [] `Null
let rre_11 = Entry.manual "11" (57.2567006216,33.6068189825) [] `Null
let rre_10 = Entry.manual "10" (55.4494311532,33.7247301749) [] `Null
let rre_9 = Entry.manual "9" (58.9267533228,26.4858948968) [] `Null
let rre_8 = Entry.manual "8" (56.4435478542,30.0813807114) [] `Null
let rre_7 = Entry.manual "7" (50.2602161381,32.3351617252) [] `Null
let rre_6 = Entry.manual "6" (55.279028622,28.1916410728) [] `Null
let rre_5 = Entry.manual "5" (53.1327984631,29.2368671763) [] `Null
let rre_4 = Entry.manual "4" (53.717220119,26.8172523776) [] `Null
let rre_3 = Entry.manual "3" (52.7292939919,26.5853731592) [] `Null
let rre_2 = Entry.manual "2" (51.6311247534,26.4973383424) [] `Null
let rre_1 = Entry.manual "1" (50.9993467265,25.3761402844) [] `Null

let r3e_10 = Entry.manual "10" (-8.39624343642,-5.43167526242) [] `Null
let r3e_9 = Entry.manual "9" (0.510260421565,8.98552359644) [] `Null
let r3e_8 = Entry.manual "8" (-3.44972362676,-7.21799188826) [] `Null
let r3e_7 = Entry.manual "7" (6.56098736978,-2.43996818291) [] `Null
let r3e_6 = Entry.manual "6" (-5.58722040722,2.18699979907) [] `Null
let r3e_5 = Entry.manual "5" (-1.85311163112,4.64391831136) [] `Null
let r3e_4 = Entry.manual "4" (-0.0600649392541,3.99954899996) [] `Null
let r3e_3 = Entry.manual "3" (-2.97301203007,-0.401496536777) [] `Null
let r3e_2 = Entry.manual "2" (-0.450354857044,1.94863554898) [] `Null
let r3e_1 = Entry.manual "1" (-0.643199593687,0.765698558625) [] `Null


let int_tree_4 = empty ()
let () = add re_10 int_tree_4
let () = add re_9 int_tree_4
let () = add re_8 int_tree_4
let () = add re_7 int_tree_4
let () = add re_6 int_tree_4
let () = add re_5 int_tree_4
let () = add re_4 int_tree_4
let () = add re_3 int_tree_4
let () = add re_2 int_tree_4
let () = add re_1 int_tree_4

let int_tree_5 = empty ()
let () = add rre_20 int_tree_5
let () = add rre_19 int_tree_5
let () = add rre_18 int_tree_5
let () = add rre_17 int_tree_5
let () = add rre_16 int_tree_5
let () = add rre_15 int_tree_5
let () = add rre_14 int_tree_5
let () = add rre_13 int_tree_5
let () = add rre_12 int_tree_5
let () = add rre_11 int_tree_5
let () = add rre_10 int_tree_5
let () = add rre_9 int_tree_5
let () = add rre_8 int_tree_5
let () = add rre_7 int_tree_5
let () = add rre_6 int_tree_5
let () = add rre_5 int_tree_5
let () = add rre_4 int_tree_5
let () = add rre_3 int_tree_5
let () = add rre_2 int_tree_5
let () = add rre_1 int_tree_5

let int_tree_6 = empty ()
let () = add r3e_10 int_tree_6
let () = add r3e_9 int_tree_6
let () = add r3e_8 int_tree_6
let () = add r3e_7 int_tree_6
let () = add r3e_6 int_tree_6
let () = add r3e_5 int_tree_6
let () = add r3e_4 int_tree_6
let () = add r3e_3 int_tree_6
let () = add r3e_2 int_tree_6
let () = add r3e_1 int_tree_6

let q1 =
  let open Prio_queue in
  let e1 = insert  9. "a" in
  let e2 = insert  3. "b" in
  let e3 = insert 18. "c" in
  let e4 = insert  0. "d" in
  empty |> e1 |> e2 |> e3 |> e4

let knn_tests = [
  (* Priority queue tests *)
  to_list_test "Simple List test" q1 ["d";"b";"a";"c"];
  extract_test "Simple List test" q1 "c";              

  (* Distance calculation tests *)
  ldist_test "Left Side" (-5.56, 0.223) rect1 4.56;
  ldist_test "Right Side" (7.62, 0.308) rect1 6.62;
  ldist_test "Top Side" (0.308, 7.62) rect1 6.62;
  ldist_test "Bottom Side" (0.223,-5.56) rect1 4.56;
  ldist_test "Upper Left" (-4., 5.) rect1 5.;
  ldist_test "Upper Right" (4., 5.) rect1 5.;
  ldist_test "Bottom Left" (-4., -5.) rect1 5.;
  ldist_test "Bottom Right" (4., -5.) rect1 5.;
  ldist_test "Inside" (0.123,-0.456) rect1 0.;

  (* k=1 *)
  knn_test "One element,k1" 1 entry_1 int_tree_1 [entry_debug];
  knn_test "Two elements,k1" 1 entry_1 int_tree_2 [entry_2];

  knn_test "10000 elements,k1" 1 entry_500k int_tree_3 [entry_500kp1];

  knn_test "Three elements,k2" 2 entry_1 int_tree_25
    [entry_2;entry_500k];

  (* N = 10, O=(0.,0.) *)
  knn_test "Random vectors,k2" 2 entry_0 int_tree_4
    [re_1;re_2];
  knn_test "Random vectors,k2" 2 entry_0 int_tree_4
    [re_1;re_2];
  knn_test "Random vectors,k7" 7 entry_0 int_tree_4
    [re_1;re_2;re_3;re_4;re_5;re_6;re_7];

  (* N = 20, O=(50.,25.34) *)
  knn_test "n20,k7" 7 entry_50 int_tree_5
    [rre_1;rre_2;rre_3;rre_4;rre_5;rre_6;rre_7];

  (* Better Distribution of data, O=(0,0) *)
  knn_test "n10,k5" 5 entry_0 int_tree_6
    [r3e_1;r3e_2;r3e_3;r3e_4;r3e_5];
]

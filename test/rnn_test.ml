open OUnit2
open Rtree
open Regression

let rnn_test
    (name : string)
    (center : Point.t)
    (radius : float)
    (tree : t)
    (loc_list : Entry.t list)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      let loc_list = 
        List.sort (fun a b -> compare (Entry.id a) (Entry.id b)) loc_list in
      tree
      |> search center radius
      |> List.sort (fun a b -> compare (Entry.id a) (Entry.id b))
      |> List.for_all2 (fun x y -> x = y) loc_list
      |> assert_equal expected_output)

(* Create tree for number of elements [elements_count] for testing rnn search 
   val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool *)
let elements_count = 100000
let center = (50000., 50000.)
let radius1 = 10.
let int_test_tree = empty ()
let () = add_from_to 0 elements_count int_test_tree
let ent_list = []

(* Create entries for search test around radius of 10 centered around point
   (50,000., 50,000.) Use pythag theorem to figure out which locations are 
   in expected location list. *)
let ent0 = Entry.manual "50007" (50007., 50007.) [] `Null
let ent1 = Entry.manual "50006" (50006., 50006.) [] `Null
let ent2 = Entry.manual "50005" (50005., 50005.) [] `Null
let ent3 = Entry.manual "50004" (50004., 50004.) [] `Null
let ent4 = Entry.manual "50003" (50003., 50003.) [] `Null
let ent5 = Entry.manual "50002" (50002., 50002.) [] `Null
let ent6 = Entry.manual "50001" (50001., 50001.) [] `Null
let ent7 = Entry.manual "50000" (50000., 50000.) [] `Null
let ent8 = Entry.manual "49999" (49999., 49999.) [] `Null
let ent9 = Entry.manual "49998" (49998., 49998.) [] `Null
let ent10 = Entry.manual "49997" (49997., 49997.) [] `Null
let ent11 = Entry.manual "49996" (49996., 49996.) [] `Null
let ent12 = Entry.manual "49995" (49995., 49995.) [] `Null
let ent13 = Entry.manual "49994" (49994., 49994.) [] `Null
let ent14 = Entry.manual "49993" (49993., 49993.) [] `Null

(* Create list from entries *)
let ent_list = ent7::ent8::ent9::ent10::ent11::ent12::ent13::ent14::ent_list
let ent_list = ent0::ent1::ent2::ent3::ent4::ent5::ent6::ent_list

(* Create entries for search test around radius of 10 centered around point
   (99,995., 99,995.) to test functionality in corner case of center being near
   maximum location point. Use pythag theorem to figure out which locations are 
   in expected location list. *)
let ent0' = Entry.manual "99999" (99999., 99999.) [] `Null
let ent1' = Entry.manual "99998" (99998., 99998.) [] `Null
let ent2' = Entry.manual "99997" (99997., 99997.) [] `Null
let ent3' = Entry.manual "99996" (99996., 99996.) [] `Null
let ent4' = Entry.manual "99995" (99995., 99995.) [] `Null
let ent5' = Entry.manual "99994" (99994., 99994.) [] `Null
let ent6' = Entry.manual "99993" (99993., 99993.) [] `Null
let ent7' = Entry.manual "99992" (99992., 99992.) [] `Null
let ent8' = Entry.manual "99991" (99991., 99991.) [] `Null
let ent9' = Entry.manual "99990" (99990., 99990.) [] `Null
let ent10' = Entry.manual "99989" (99989., 99989.) [] `Null
let ent11' = Entry.manual "99988" (99988., 99988.) [] `Null

(* Create list from entries *)
let center2 = (99995., 99995.)
let ent_list' = []
let ent_list' = ent6'::ent7'::ent8'::ent9'::ent10'::ent11'::ent_list' 
let ent_list' = ent0'::ent1'::ent2'::ent3'::ent4'::ent5'::ent_list'

(* Create entries for search test around radius of 10 centered around point
   (0., 0.) to test functionality in corner case of center being near
   minimum location point. Use pythag theorem to figure out which locations are 
   in expected location list. *)
let ent0'' = Entry.manual "7" (7., 7.) [] `Null
let ent1'' = Entry.manual "6" (6., 6.) [] `Null
let ent2'' = Entry.manual "5" (5., 5.) [] `Null
let ent3'' = Entry.manual "4" (4., 4.) [] `Null
let ent4'' = Entry.manual "3" (3., 3.) [] `Null
let ent5'' = Entry.manual "2" (2., 2.) [] `Null
let ent6'' = Entry.manual "1" (1., 1.) [] `Null
let ent7'' = Entry.manual "0" (0., 0.) [] `Null

(* Create list from entries *)
let center3 = (0., 0.)
let ent_list'' = []
let ent_list'' = ent4''::ent5''::ent6''::ent7''::ent_list'' 
let ent_list'' = ent0''::ent1''::ent2''::ent3''::ent_list''

let rnn_tests = [
  rnn_test "radius 10, center (50,000., 50,000.)" 
    center radius1 int_test_tree ent_list true;
  rnn_test "radius 10, center (99,995., 99,995.), upper corner case"
    center2 radius1 int_test_tree ent_list' true;
  rnn_test "radius 10, center (0., 0.), lower corner case"
    center3 radius1 int_test_tree ent_list'' true;
]



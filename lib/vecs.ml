(* This is a simple utility to help generate randomized test cases
   for the knn search. It is used to produce hard-coded test cases
   of randomized inputs to allow for ease in debugging and determinisic
   test cases on random data.
*)

(* (x,y) *)
type vector = float * float

(* Generates a random vector from the origin *)
let random_vec () =
  let open Random in
  self_init ();
  (float 100., float 100.)

let mag (x,y) = sqrt ((x ** 2.) +. (y ** 2.))

let unit (x,y) =
  let mag' = mag (x,y) in
  (x /. mag', y /. mag')

(* [rvec origin r] generates a random vector with
   origin origin of length r
   with 0<=origin<=100 *)
let rvec (x,y) r () =
  let v = random_vec () in
  let (ux, uy) = unit v in
  let (sx, sy) = (r *. ux, r *. uy) in
  (x +. sx, y +. sy)

(* Generate a list of random vectors with increasing length about v*)
let vecl v n () =
  let rec aux k acc () =
    match k = 0 with
    | true -> acc
    | false -> aux (k-1) ((rvec v (float_of_int k) ())::acc) () in
  aux n [] ()

let test_case vl =
  let make_elem id (x,y) =
    let (xs,ys) = (string_of_float x, string_of_float y) in
    "let entry_"^id^{| = Entry.manual "|}^id^{|" (|}^xs^","^ys^") [] `Null" in
  let rec aux vl c acc =
    match vl with
    | [] -> acc
    | v::vls -> aux vls (c+1) ((make_elem (string_of_int c) v)::acc) in
  aux vl 1 []

(* n random test cases about O *)
let rtn o n () =
  vecl o n () |> test_case
  

(* List of 10 Random vectors generated with rtn about (0.,0.)
let re_10 = Entry.manual "10"(5.14978927308,8.5720283739) [] `Null;
let re_9 = Entry.manual "9" (8.69086653949,2.33855485136) [] `Null;
let re_8 = Entry.manual "8" (7.65397083313,2.32738705109) [] `Null;
let re_7 = Entry.manual "7" (6.99899986609,0.118325291041) [] `Null;
let re_6 = Entry.manual "6" (5.63907297497,2.049598981) [] `Null;
let re_5 = Entry.manual "5" (4.4629101228,2.25442525621) [] `Null;
let re_4 = Entry.manual "4" (0.851489653826,3.90832001881) [] `Null;
let re_3 = Entry.manual "3" (1.94751670357,2.28192433909) [] `Null;
let re_2 = Entry.manual "2" (0.227701987768,1.98699567306) [] `Null;
let re_1 = Entry.manual "1" (0.999518816636,0.0310183041281) [] `Null;

List of 20 Random vectors generated with rtn about (50.,25.34.)
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
 *)

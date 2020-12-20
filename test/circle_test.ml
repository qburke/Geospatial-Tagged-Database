open OUnit2
open Point
open Rect
open Circle

let center1 = Point.origin
let radius1 = 3.

(* each rect in one quadran (I, II, III, IV) *)
let rect_A    = ((2.,1.),   (6.,5.))
let rect_B    = ((2.,-5.),  (6.,-1.))
let rect_C    = ((-6.,-5.), (-2.,-1.))
let rect_D    = ((-6.,1.),  (-2.,5.))

(* stretch quadran I to II, II to I, III to IV, IV to III *)
let rect_A'   = ((2.,-1.),  (6.,5.))
let rect_B'   = ((2.,-5.),  (6.,1.))
let rect_C'   = ((-6.,-5.), (-2.,1.))
let rect_D'   = ((-6.,-1.),  (-2.,5.))

(* stretch quadran I to IV, II to III, III to II, IV to I *)
let rect_A''  = ((-1.,1.),   (6.,5.))
let rect_B''  = ((-1.,-5.),  (6.,-1.))
let rect_C''  = ((-6.,-5.),  (1.,-1.))
let rect_D''  = ((-6.,1.),   (1.,5.))

(* each rect in one quadran (I, II, III, IV) touches the perimeter *)
let rect_I    = ((3.,-2.),   (7.,2.))
let rect_J    = ((-7.,-2.),  (-3.,2.))
let rect_K    = ((-2.,-7.), (2.,-3.))
let rect_L    = ((-2.,3.),  (2.,7.))

(* no intersection points for quadran I - IV *)
let rect_E    = ((3.,1.),   (6.,5.))
let rect_F    = ((3.,-5.),  (6.,-1.))
let rect_G    = ((-6.,-5.), (-3.,-1.))
let rect_H    = ((-6.,1.),  (-3.,5.))


let center2 = (100.,100.)
let radiu2 = 3.
let rect_P    = ((102., 101.), (106., 105.))
let rect_Q    = ((102., 95.),  (106., 99.))
let rect_R    = ((94.,  95.),  (98.,99.))
let rect_S    = ((94., 101.),  (98.,105.))

let rect_P'   = ((102.,99.), (106.,105.))
let rect_Q'   = ((102.,95.), (106.,101.))
let rect_R'   = ((94.,95.), (98.,101.))
let rect_S'   = ((94.,99.),  (98.,105.))

let rect_P''  = ((99.,101.), (106.,105.))
let rect_Q''  = ((99.,95.),   (106.,99.))
let rect_R''  = ((94.,95.),   (101.,99.))
let rect_S''  = ((94.,101.), (101.,105.))

(* Not at center, no intersection points *)
let rect_T    = ((103.,101.),   (106.,105.))
let rect_U    = ((103.,95.),  (106.,99.))
let rect_V    = ((94.,95.), (97.,99.))
let rect_W    = ((94.,101.),  (97.,105.))

(* each rect in one quadrant (I, II, III, IV) touches the perimeter *)
let rect_I'    = ((103.,98.), (107.,102.))
let rect_J'    = ((93.,98.),   (97.,102.))
let rect_K'    = ((98.,93.),  (102.,97.))
let rect_L'    = ((98.,103.), (102.,107.))

let intersect_circle_test
    (name : string)
    (center : Point.t)
    (radius : float)
    (rect : Rect.t)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal expected_output 
        (intersect center radius rect))

let circle_tests = [

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant I"   
    center1 radius1 rect_A true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant II"  
    center1 radius1 rect_B true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant III" 
    center1 radius1 rect_C true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant IV"  
    center1 radius1 rect_D true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant I & II"    
    center1 radius1 rect_A' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant II & I"    
    center1 radius1 rect_B' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant III & IV"  
    center1 radius1 rect_C' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant IV & III" 
    center1 radius1 rect_D' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant I & IV"    
    center1 radius1 rect_A'' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant II & III"  
    center1 radius1 rect_B'' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant III & II"  
    center1 radius1 rect_C'' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant IV  & I"   
    center1 radius1 rect_D'' true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant I, outside the radius"  
    center1 radius1 rect_E false;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant II, outside the radius"  
    center1 radius1 rect_F false;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant III, outside the radius"  
    center1 radius1 rect_G false;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect in quadrant IV, outside the radius"  
    center1 radius1 rect_H false;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect touches at the right perimeter"  
    center1 radius1 rect_I true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect touches at the left perimeter" 
    center1 radius1 rect_J true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect touches at the bottom perimeter" 
    center1 radius1 rect_K true;

  intersect_circle_test 
    "Center at origin, radius 3, 
    rect touches at the top perimeter"
    center1 radius1 rect_L true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant I"   
    center2 radius1 rect_P true;

  intersect_circle_test 
    "Center at (100.,100.), 
    adius 3, rect in quadran II"  
    center2 radius1 rect_Q true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant III" 
    center2 radius1 rect_R true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadran IV"
    center2 radius1 rect_S true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant I & II"
    center2 radius1 rect_P' true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant II & I"
    center2 radius1 rect_Q' true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant III & IV"
    center2 radius1 rect_R' true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant IV & III" 
    center2 radius1 rect_S' true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant I & IV"
    center2 radius1 rect_P'' true;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    rect in quadran II & III"  
    center2 radius1 rect_Q'' true;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    rect in quadran III & II"  
    center2 radius1 rect_R'' true;

  intersect_circle_test 
    "Center at (100.,100.), 
    radius 3, rect in quadrant IV & I"
    center2 radius1 rect_S'' true;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    outside the radius right"
    center2 radius1 rect_T false;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    outside the radius left"
    center2 radius1 rect_U false;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    outside the radius top"    
    center2 radius1 rect_V false;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    outside the radius bottom" 
    center2 radius1 rect_W false;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    rect touches at the right perimeter"  
    center2 radius1 rect_I' true;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    rect touches at the left perimeter"  
    center2 radius1 rect_J' true;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    rect touches at the bottom perimeter" 
    center2 radius1 rect_K' true;

  intersect_circle_test 
    "Center at (100.,100.), radius 3, 
    rect touches at the top perimeter"    
    center2 radius1 rect_L' true;

]
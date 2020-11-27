open Lib

val rect_tests : OUnit2.test list 

(** [enlargement_rect_test name r1 r2 expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [enlargement_rect r1 r2]. *)
val enlargement_rect_test : string -> Rect.t -> Rect.t -> Rect.t -> OUnit2.test

(** [mbr_of_list_test name lst expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [mbr_of_list lst]. *)
val mbr_of_list_test : string -> Rect.t list -> Rect.t -> OUnit2.test

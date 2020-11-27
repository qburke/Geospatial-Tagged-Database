open Lib

(** [list_of_tags_test name db expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [list_of_reverse_index db]. *)
val list_of_tags_test : string -> Db.database -> string list -> OUnit2.test

(** [tag_contents_test name db tag expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [list_of_tag_collection db tag]. *)
val tag_contents_test : string -> Db.database -> string -> 
  string list -> OUnit2.test

(** [tag_search_test name db objects tags expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [tag_search_test db objects tags]. *)
val tag_search_test : string -> Db.database -> Db.element list -> 
  string list -> string list -> OUnit2.test

val db_tests : OUnit2.test list
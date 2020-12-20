open Lib

val knn_tests : OUnit2.test list

val ldist_test : string -> Point.t -> Rect.t -> float -> OUnit2.test

val knn_test : string -> int -> Entry.t -> Rtree.t -> Entry.t list 
  -> OUnit2.test

val to_list_test : string -> 'a Prio_queue.queue -> 'a list -> OUnit2.test

val extract_test : string -> 'a Prio_queue.queue -> 'a -> OUnit2.test
open Rtree

(** [ints_from_to x0 x1] is a list of increasing 
    ints from [x0] to [x1] exclusive. *)
val ints_from_to : int -> int -> int list

(** [entries_of_int_range lst] is a list of floats
    from "lst" a list of integers *)
val entries_of_int_range : int list -> ((float * float) * int) list

(** [floats_from_to lb ub] is the list of floats 
    from [lb] to [ub] exclusive. *)
val floats_from_to : int -> int -> ((float * float) * int) list

(** [add_from_to lb ub t] inserts int values to tree [t] 
    from [lb] to [ub]; if lb < ub, points are inserted in 
    increasing order, otherwise inserted in decreasing order. *)
val add_from_to : int -> int -> int Rtree.t -> unit

(** [add_random range n t] inserts to tree [t]
    [n] random entries between base 0 and a given [range] *)
val add_random : int -> int -> int Rtree.t -> unit

(** [add_cluster mid rad n t] inserts to tree [t] 
    [n] random entries within a given radius [rad] around point [mid]. *)
val add_cluster : int -> int -> int -> int Rtree.t -> unit
(** Priority Queue implementation taken from 
    http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html 
    Modified to store elements in descending, rather than ascending
    order. as well as some function argument orders*)

(** TODO *)
type priority = float

(** TODO *)
type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

(** TODO *)                          
val empty : 'a queue

(** TODO *)    
val insert : priority -> 'a -> 'a queue -> 'a queue

exception Queue_is_empty

(** TODO *)
val remove_top : 'a queue -> 'a queue

(** TODO *)
val extract : 'a queue -> priority * 'a * 'a queue

(* Original Code starts here *)

(** [to_list q] returns a list of the entries of q in ascending
    order *)
val to_list : 'a queue -> (priority * 'a) list

(** Priority Queue implementation taken from 
    http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html 
    Modified to store elements in descending, rather than ascending
    order. as well as some function argument orders*)

(** [priority] is the type of priority for the queue. A smaller value means
    higher priority. *)
type priority = float

(** [queue] is the type of priority queues *)
type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

(** [empty] is the empty priority queue *)                          
val empty : 'a queue

(** [insert p elt q] is the queue with [elt] inserted into [q] with priority
    [p]. *)    
val insert : priority -> 'a -> 'a queue -> 'a queue

exception Queue_is_empty

(** [remove_top q] is the queue with the highest priority element removed *)
val remove_top : 'a queue -> 'a queue

(** [extract q] is the equivalent of pop for the priority queue [q] *)
val extract : 'a queue -> priority * 'a * 'a queue

(* Original Code starts here *)

(** [to_list q] returns a list of the entries of q in ascending
    order *)
val to_list : 'a queue -> (priority * 'a) list

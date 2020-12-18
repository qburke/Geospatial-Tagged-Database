(** Priority Queue implementation taken from 
    http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html *)

type priority = float
  
type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
                                  
val empty : 'a queue
    
val insert : 'a queue -> priority -> 'a -> 'a queue
    
exception Queue_is_empty
  
val remove_top : 'a queue -> 'a queue
    
val extract : 'a queue -> priority * 'a * 'a queue

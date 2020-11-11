open Command

(** 
   Representation of interactive database state.

   This module represents the state of the database interface,
   including the current loaded database, and the format of the data 
   entries.
*)

(** The abstract type of values representing the database state *)
type 'a t

exception DatabaseAlreadyExists
  
exception NoDatabaseInitialized

exception DataNotFound
  
(** [init_state] is the initial state of the interface.
    In that state, no database has been loaded *)
val init_state : 'a t

(** TODO: Add rest of functionality found in command.mli *)
val add : 'a t -> 'a -> string list -> Point.t -> unit

val delete_elem : 'a t -> 'a -> unit

val get_elems : 'a t -> ('a * Point.t * string list) list

val get_tags : 'a t -> string list

(** [help param] returns a string corresponding to a help phrase
      for the given object phrase *)
val help : object_phrase -> string

(**  [initialize name dummy] returns a state containing an initialized
     database *)
val initialize : 'a t -> string -> string t

val query_elems : 'a t -> string list -> ('a * Point.t * string list) list

val is_initialized : 'a t -> bool

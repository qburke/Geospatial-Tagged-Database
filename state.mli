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

(**  [initialize name dummy] returns a state containing an initialized
     database *)
val initialize : 'a t -> string -> string t

(** [init_state] is the initial state of the interface.
    In that state, no database has been loaded *)
val init_state : 'a t

(** [query_elems  st tags] returns a tuple of element components that 
match the given list of [tags] *)
val query_elems : 'a t -> string list -> ('a * Point.t * string list) list

(** [add st data tags location] creates a new element and
    adds it to [st]. Raises an exception if [st] is uninitialized. *)
val add : 'a t -> 'a -> string list -> Point.t -> unit

(** [delete_elem st n] deletes an element with name [n] from [st].
    Raises an exception if the element is not found in the [st] or
    if st is not initialized. *)
val delete_elem : 'a t -> 'a -> unit

(** [get_elems st] returns a list of tuples of the components of the
    elements in [st]. Raises an exception if [st] is unitialized *)
val get_elems : 'a t -> ('a * Point.t * string list) list

(** [get_tags st] returns a list of tags in [st].
    Raises an exception if [st] is unitialized *)
val get_tags : 'a t -> string list

(** [help param] returns a string corresponding to a help phrase
      for the given object phrase *)
val help : object_phrase -> string




val is_initialized : 'a t -> bool

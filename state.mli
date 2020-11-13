open Command

(** 
   Representation of interactive database state.

   This module represents the state of the database interface,
   including the current loaded database, and the format of the data 
   entries.
*)

(** The abstract type of values representing the database state *)
type t

exception DatabaseAlreadyExists

exception NoDatabaseInitialized

exception DataNotFound

(**  [initialize name dummy] returns a state containing an initialized
     database *)
val initialize : t -> string -> t

(** [init_state] is the initial state of the interface.
    In that state, no database has been loaded *)
val init_state : t

(** [add st data tags location] creates a new element and
    adds it to [st]. Raises an exception if [st] is uninitialized. *)
val add : t -> string -> string list -> Point.t -> Yojson.Basic.t -> unit

(** [get_elems st] returns a list of tuples of the components of the
    elements in [st]. Raises an exception if [st] is unitialized *)
val get_elems : t -> (string * Point.t * string list) list

(** [query_elems  st tags] returns a tuple of element components that 
    match the given list of [tags] *)
val query_elems : t -> string list -> (string * Point.t * string list) list

(** [delete_elem st n] deletes an element with name [n] from [st].
    Raises an exception if the element is not found in the [st] or
    if st is not initialized. *)
val delete_elem : t -> string -> unit

(** [get_tags st] returns a list of tags in [st].
    Raises an exception if [st] is unitialized *)
val get_tags : t -> string list

(** [help param] returns a string corresponding to a help phrase
      for the given object phrase *)
val help : object_phrase -> string

val is_initialized : t -> bool

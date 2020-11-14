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

exception TagNotFound

exception InvalidFileName
(**  [initialize name dummy] returns a state containing an initialized
     database *)
val initialize : t -> string -> t

(** [init_state] is the initial state of the interface.
    In that state, no database has been loaded *)
val init_state : t

(** [add st data tags location] creates a new element and
    adds it to [st]. Raises an exception if [st] is uninitialized. *)
val add : t -> string -> string list -> Point.t -> Yojson.Basic.t -> unit

(** [get_elems st v] returns a list of the string representation of the
    elements in the database. Prints out verbose data if [v] is true *)
val get_elems : t -> bool -> string list

(** [query_elems st v tags] returns a string list of elements that
    match the given list of tags *)
val query_elems : t -> bool ->  string list -> string list

(** [delete_elem st n] deletes an element with name [n] from [st].
    Raises an exception if the element is not found in the [st] or
    if st is not initialized. *)
val delete_elem : t -> string -> unit

(** [get_tags st] returns a list of tags in [st].
    Raises an exception if [st] is unitialized *)
val get_tags : t -> string list

(** [load_db st f name] returns a new Database db created from
    [f]. Raises an exception if [f] is not a valid json file. *)
val load_db : t -> string -> string -> t

(** [write_db st format f] writes the contents of db created from
    [f]. The [format] of the json file can be either a flattened [list]
    or an [rtree]. 
    Raises an exception if no database is initialized in [st] or
    if [f] is not a valid json filename. *)
val write_db : t -> string -> string -> unit

(** [help param] returns a string corresponding to a help phrase
      for the given object phrase *)
val help : object_phrase -> string

val is_initialized : t -> bool

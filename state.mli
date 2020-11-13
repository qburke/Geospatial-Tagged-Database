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

(** [init_state] is the initial state of the interface.
    In that state, no database has been loaded *)
val init_state : t

(** TODO: Add rest of functionality found in command.mli *)
val add : t -> string -> string list -> Point.t -> Yojson.Basic.t -> unit

val get_elems : t -> (string * Point.t * string list) list

(** [help param] returns a string corresponding to a help phrase
      for the given object phrase *)
val help : object_phrase -> string

(**  [initialize name dummy] returns a state containing an initialized
     database *)
val initialize : t -> string -> t

val query_elems : t -> string list -> (string * Point.t * string list) list

val is_initialized : t -> bool

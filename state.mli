(** 
   Representation of interactive database state.

   This module represents the state of the database interface,
   including the current loaded database, and the format of the data 
   entries.
*)

(** The abstract type of values representing the database state *)
type 'a t

(** [init_state] is the initial state of the interface.
    In that state, no database has been loaded *)
val init_state : 'a t

(** TODO: Add rest of functionality found in command.mli *)

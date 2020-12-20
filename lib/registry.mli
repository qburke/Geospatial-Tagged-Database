(** A mutable registry of active / loaded databases *)

(** type t is the type of database registry *)
type t

exception DbAlreadyLoaded

(** [init ()] is a new empty database registry. *)
val init : unit -> t

(** [load_from_file r file name] loads the database from [file] into registry
    [r] with name [name] and returns the key by which to fetch the newly loaded
    database. If the database is already loaded in [r], it will be treated as a
    cloned instance of the database. *)
val load_from_file : t -> string -> string -> string

(** [add_new r name] adds a new empty database to the registry [r] named [name]
    and returns the key by which to fetch the newly loaded database. *)
val add_new : t -> string -> string

(** [get r k] is the database corresponding to key [k] in registry [r]. *)
val get :  t -> string -> Db.database
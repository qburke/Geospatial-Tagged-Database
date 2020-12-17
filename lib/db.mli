(** ['a element] is the type of an element of the database.
        which contains data of type ['a] and a set of tags associated
        with it  of type [string list] *)
type element

(** ['a spatial_data] is a mutable datastructure for storing elements of
         type ['a] *)
type spatial_data

(** ['a tag_collection] is a mutable set of elements
    of type ['a element]. *)
type tag_collection

(** ['a reverse_index] is a mutable map from strings to elements of
    type ['a tag_collection] *)
type reverse_index

(** ['a database] contains a spatial datastructure and a reverse_index for
    elements of type [a'] *)
type database

(** [create data tags] returns an ['a element] *)
val create_element : string -> Point.t -> string list -> Yojson.Basic.t -> element

val id_of_element : element -> string

(* FIXME *)
val find : database -> string -> element

val data_of_element : element -> Yojson.Basic.t

val entry_of_element : element -> Entry.t

val tags_of_element : element -> string list

val location_of_element : element -> Point.t

val create_db : string -> database 

val list_of_reverse_index : database -> string list

val list_of_tag_collection : database -> string -> element list

val list_of_elements : database -> element list

(** [add db e]  adds the data of type ['a] of an item of type ['a element] to
    the [reverse_index] of [db] for each tag_collection corresponding to the tags of
    [e]. If there is a tag not present in [reverse_index] then a new tag collection is
    created and added to [reverse_index] with the new tag as the key. *)
val add : database -> element -> unit

(** [remove db e] removes the data represented by [e] from the database *)
val remove : database -> element -> unit

(** [tag_search db objects tags] takes a collection of objects of type ['a element list] 
    and returns a list of elements which match all of the tags in the [string list] *)
val tag_search : database -> element list -> string list -> element list

(** [string_of_element e] returns the string representation of [e]. Prints
    verbose data if [v] is true *)
val string_of_element : bool -> element -> string

(** [from_json f name] creates a database with name [name] from the database
    stored in [f] *)
val from_json : string -> string -> database

(** [to_rtree_json db f] creates a JSON file with filename [f] from the database
    stored in [db], following the internal R Tree structure *)
val to_rtree_json : database -> string -> unit

(** [to_list_json db f] creates a JSON file with filename [f] from the database
    stored in [db], in a flattened list structure *)
val to_list_json : database -> string -> unit
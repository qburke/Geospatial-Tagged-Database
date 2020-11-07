(** ['a element] is the type of an element of the database.
        which contains data of type ['a] and a set of tags associated
        with it  of type [string list] *)
type 'a element

(** ['a spatial_data] is a mutable datastructure for storing elements of
         type ['a] *)
type 'a spatial_data

(** ['a tag_collection] is a mutable set of elements
    of type ['a element]. *)
type 'a tag_collection

(** ['a reverse_index] is a mutable map from strings to elements of
    type ['a tag_collection] *)
type 'a reverse_index

(** ['a database] contains a spatial datastructure and a reverse_index for
    elements of type [a'] *)
type 'a database

(** [create data tags] returns an ['a element] *)
val create_element : 'a -> string list -> Point.t -> 'a element

val data_of_element : 'a element -> 'a

val create_db : string -> 'a element -> 'a database 

val list_of_reverse_index : 'a database -> string list

val list_of_tag_collection : 'a database -> string -> 'a element list
(** [add db e]  adds the data of type ['a] of an item of type ['a element] to
    the [reverse_index] of [db] for each tag_collection corresponding to the tags of
    [e]. If there is a tag not present in [reverse_index] then a new tag collection is
    created and added to [reverse_index] with the new tag as the key. *)
val add : 'a database -> 'a element -> unit

(** [tag_search db objects tags] takes a collection of objects of type ['a element list] 
    and returns a list of elements which match all of the tags in the [string list] *)
val tag_search : 'a database -> 'a element list -> string list -> 'a element list

(** An entry requiring an id, location, and tags *)

(** [type t] is the type of entries*)
type t

(** [from_json json] is the Entry constructed from [json]. Raises an exception
    in the case of poorly formatted JSON or if any of the attributes "id",
    "tags", or "mbr" are missing. *)
val from_json : Yojson.Basic.t -> t

(** [manual id mbr tags data] is the Entry with id [id], mbr [mbr], tags [tags],
    and with additional the attributes included in [data]. If no additional
    attributes are desired, [data] should be [`Null]. [data] should not include
    any of the attributes "id", "tags", or "mbr" ([Entry.from_json] exists for
    this purpose.) *)
val manual : string -> Point.t -> string list -> Yojson.Basic.t -> t

(** [id entry] is the id of [entry]. *)
val id : t -> string

(** [loc entry] is the point location of [entry]. *)
val loc : t -> Point.t

(** [mbr entry] is the mbr of [entry]. *)
val mbr : t -> Rect.t

(** [tags entry] is the list of tags of [entry]. *)
val tags : t -> string list

(** [data entry] is the JSON data of [entry] *)
val data : t -> Yojson.Basic.t

(** [to_json entry] is the JSON representation of [entry]. *)
val to_json : t -> Yojson.Basic.t

(** [to_string v entry] is the string representation of [entry]. If [v] is false,
    the string will omit the entry data.*)
val to_string : bool -> t -> string

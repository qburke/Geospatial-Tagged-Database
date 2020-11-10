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
val manual : string -> Rect.t -> string list -> Yojson.Basic.t -> t

(** [id entry] is the id of [entry]. *)
val id : t -> string

(** [mbr entry] is the mbr of [entry]. *)
val mbr : t -> Rect.t

(** [tags entry] is the list of tags of [entry]. *)
val tags : t -> string list

(** [to_json entry] is the JSON representation of [entry]. *)
val to_json : t -> Yojson.Basic.t
(** [t] is the type representing geospatial coordinate points. *)
type t = float * float

(** The origin point. *)
val origin : t

(** [to_json t] is the JSON representation of [t]. See the R Tree schema. *)
val to_json : t -> Yojson.Basic.t

(** [to_string p] is the string representation of [p] *)
val to_string : t -> string
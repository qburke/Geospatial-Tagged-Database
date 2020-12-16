(** [t] is the type representing geospatial coordinate points. *)
type t = float * float

(** The origin point. *)
val origin : t

(** [to_json t] is the JSON representation of [t]. See the R Tree schema. *)
val to_json : t -> Yojson.Basic.t

(** [to_string p] is the string representation of [p] *)
val to_string : t -> string

(** [distance p1 p2] is distance between point [p1] and point [p2]. *)
val distance : t -> t -> float
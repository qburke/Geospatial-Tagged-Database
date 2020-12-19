(** The abstract type of values representing Rects. *)
type t = Point.t * Point.t

(** [empty] is an empty Rect representation. *)
val empty : t

(** [ll t] is the lower left corner of [t]. *)
val ll : t -> Point.t

(** [ur t] is the upper right corner of [t]. *)
val ur : t -> Point.t

(** [is_in] checks if [pt] is inside of [rect].*)
val is_in : Point.t -> t -> bool

(** [contains r1 r2] is true if r2 is contained in r1. *)
val contains : t -> t -> bool

(** [of_point p] is the MBR containing point [p].*)
val of_point : Point.t -> t

(** [area rect] calculates the area of [rect].*)
val area : t -> float

(** [perimeter rect] calculates the perimeter of [rect].*)
val perimeter : t -> float

(** [enlargement_rect rect1 rect2] is the new bounding box and the 
    after adding [rect2] to [rect1]. *)
val enlargement_rect : t -> t -> t

(** [mbr_of_list lst] is the MBR of the the rectangles in [lst].*)
val mbr_of_list : t list -> t

(** [to_json r] is the JSON representation of [r] *)
val to_json : t -> Yojson.Basic.t

(** [to_string r] is the string representation of [r] *)
val to_string : t -> string


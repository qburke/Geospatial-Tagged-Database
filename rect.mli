open Point

(** The abstract type of values representing Rects. *)
type t = Point.t * Point.t

(** [empty] is an empty Rect representation. *)
val empty : t

(** [is_in] checks if [pt] is inside of [rect].*)
val is_in : Point.t -> t -> bool

(** [area rect] calculates the area of [rect].*)
val area : t -> float

(** [enlargement_pt (x0, y0) ((x0', y0'), (x1', y1'))] checks if 
    pt [(x0, y0)] is in rect [((x0', y0'), (x1', y1'))] 
    and returns enlargement area and new or existing rect. *)
val enlargement_pt : Point.t -> t -> t * float

(** [enlargement_rect ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1'))]   
    calculates enlargement area from adding rect' to rect and 
    the new rect containing both.*)
val enlargement_rect : t -> t -> t * float


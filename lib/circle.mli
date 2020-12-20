(** Radius-based calculations for coordinate circles *)

(** [contains c r p] is true if p is contained in circle of radius [r] with
    center [c]. *)
val contains : Point.t -> float -> Point.t -> bool

(** [intersect c r rect] is true if circle with center [c], 
    radius [r] intersects with rect [r]. *)
val intersect : Point.t -> float -> Rect.t -> bool


(** [intersect c r rect] is true if circle with center [c], 
    radius [r] intersects with rect [r]. *)
val intersect : Point.t -> float -> Rect.t -> bool
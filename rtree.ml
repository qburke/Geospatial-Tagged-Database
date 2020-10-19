module Point = struct
  (** AF: (x, y) represents the point at (x, y). *)
  type t = float * float

end

module Rect = struct
  (** AF: (p1, p2) is the rectangle with p1 as the bottom left corner
      		and p2 as the upper right corner. *)
  type t = Point.t * Point.t

  (** *)
  let intersect a b a' b' = a' <= b && a <= b'

end

module type RTree = sig

  (** ['a t] is the type of R Trees whose elements have type ['a]. 
      The R Tree is mutable in the sense that the operations [add]
      and [remove] destructively modify the R Tree.
      AF: *)
  type 'a t

  (** [empty ()] is the empty R Tree. *)
  val empty : 'a t

  (** [add x r] modifies [r] to include [x]. Requires [x] not in [r]. *)
  val add : 'a -> 'a t -> unit

  (** [remove x r] removes [x] from [r]. Leaves [r] unchanged if [x]
      not in [r]. *)
  val remove : 'a -> 'a t -> unit

  (** [union r1 r2] returns the set union of [r1] and [r2].
      [r1] contains less than or equal to elements than [r2].
      [r1] and [r2] are not allowed to contain duplicates *)
  val union : 'a t -> 'a t -> 'a t

  (** [inter r1 r2] returns the set intersection of [r1] and [r2].
      	[r1] contains less than or equal to elements than [r2].	
      [r1] and [r2] are not allowed to contain duplicates *)
  val inter : 'a t -> 'a t -> 'a t

  (*read and conversion operations*)
end

module RTree1 : RTree = struct

  type 'a t = 
    | Node of (Rect.t * 'a t) list
    | Leaf of (Point.t * 'a) list
    | Empty

  let empty = Empty

  let add = failwith "Unimplemented"

  let remove = failwith "Unimplemented"

  let union = failwith "Unimplemented"

  let inter = failwith "Unimplemented"

end
open Point
open Rect

type 'a t = 
  | Node of (Rect.t * 'a t) list
  | Leaf of (Point.t * 'a) list
  | Empty

let empty = Empty

let add = failwith "Unimplemented"

let remove = failwith "Unimplemented"

let union = failwith "Unimplemented"

let inter = failwith "Unimplemented"


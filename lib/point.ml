type t = float * float

let origin = (0., 0.)

let to_json (t : t) : Yojson.Basic.t =
  `Assoc [("x", `Float (fst t)); ("y", `Float (snd t))]

let to_string (x, y) = "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

(** [distance p1 p2] is distance between point [p1] and point [p2]. *)
let distance (x0,y0) (x1,y1) =
  Float.hypot (x0 -. x1) (y0 -. y1)

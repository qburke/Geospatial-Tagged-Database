type t = float * float

let origin = (0., 0.)

let to_json (t : t) : Yojson.Basic.t =
  `Assoc [("x", `Float (fst t)); ("y", `Float (snd t))]

let to_string (x, y) = 
  "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

let distance (x0,y0) (x1,y1) =
  Float.hypot (x0 -. x1) (y0 -. y1)



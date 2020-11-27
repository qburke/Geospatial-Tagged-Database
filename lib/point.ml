type t = float * float

let origin = (0., 0.)

let to_json (t : t) : Yojson.Basic.t =
  `Assoc [("x", `Float (fst t)); ("y", `Float (snd t))]

let to_string (x, y) = "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"
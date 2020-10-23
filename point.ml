type t = float * float

let origin = (0., 0.)

let to_json (t : t) : Yojson.Basic.t = Yojson.Basic.(
    `Assoc [("x", `Float (fst t)); ("y", `Float (snd t))]
  )
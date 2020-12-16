open Point
open Rect

let contains c r p =
  r >= distance c p

let intersect (cx,cy) radius ((x0,y0),(x1,y1)) =
   let w_half = (x1 -. x0) /. 2. in
   let h_half = (y1 -. y0) /. 2. in
   let (rc_x, rc_y) = ((x0 +. w_half),(y0 +. h_half)) in
   let dx = Float.abs (cx -. rc_x) -. w_half in
   let dy = Float.abs (cy -. rc_y) -. h_half in
   if dx > radius || dy > radius then false
   else if dx <= 0. || dy <= 0. then true
   else (dx *. dx +. dy *. dy) <= radius *. radius


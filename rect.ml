open Point

(** AF: (p1, p2) is the rectangle with p1 as the bottom left corner
    		and p2 as the upper right corner. *)
type t = Point.t * Point.t

let empty = (Point.origin, Point.origin)

let is_in (x0, y0) ((x0', y0'), (x1', y1')) =
  let x_in = x0 >= x0' && x0 <= x1' in
  let y_in = y0 >= y0' && y0 <= y1' in 
  x_in && y_in

let area ((x0', y0'), (x1', y1')) =
  let x_len = x1' -. x0' in
  let y_len = y1' -. x0' in
  x_len *. y_len

let enlargement_pt (x0, y0) ((x0', y0'), (x1', y1')) = 
  match is_in (x0, y0) ((x0', y0'), (x1', y1')) with
  | true -> ((x0', y0'), (x1', y1')), 0.
  | false -> let new_rect = ((min x0 x0', min y0 y0'), 
                             (max x0 x1', max y0 y1')) 
    in new_rect, area new_rect -. area ((x0', y0'), (x1', y1'))

let enlargement_rect ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) = 
  let new_rect = ((min x0 x0', min y0 y0'), (max x1 x1', max y1 y1')) 
  in new_rect, area new_rect -. area ((x0, y0), (x1, y1))


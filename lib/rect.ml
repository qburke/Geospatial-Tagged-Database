(** AF: (p1, p2) is the rectangle with p1 as the bottom left corner
    		and p2 as the upper right corner. *)
type t = Point.t * Point.t

let empty = (Point.origin, Point.origin)

let ll t = fst t

let ur t = snd t

let is_in (x0, y0) ((x0', y0'), (x1', y1')) =
  let x_in = x0 >= x0' && x0 <= x1' in
  let y_in = y0 >= y0' && y0 <= y1' in 
  x_in && y_in

let contains r (p1, p2) =
  is_in p1 r && is_in p2 r

let area ((x0', y0'), (x1', y1')) =
  let x_len = x1' -. x0' in
  let y_len = y1' -. y0' in
  x_len *. y_len

let perimeter ((x0, y0), (x1, y1)) =
  let x_len = x1 -. x0 in
  let y_len = y1 -. y0 in
  x_len +. y_len |> ( *. ) 2.0

let of_point p = (p, p)

(** [enlargement_pt p rect] is the new rectangle and the difference
    in area by adding [p] within rectanle [rect] *)
(*let enlargement_pt (x0, y0) ((x0', y0'), (x1', y1')) = 
  match is_in (x0, y0) ((x0', y0'), (x1', y1')) with
  | true -> ((x0', y0'), (x1', y1')), 0.
  | false -> let new_rect = ((min x0 x0', min y0 y0'), 
                             (max x0 x1', max y0 y1')) 
    in new_rect, area new_rect -. area ((x0', y0'), (x1', y1'))*)


(** [enlargement_rect rect1 rect2] is the new bounding box by adding [rect2] to
    [rect1]. *)
let enlargement_rect ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) = 
  ((min x0 x0', min y0 y0'), (max x1 x1', max y1 y1'))

let mbr_of_list (lst : t list) : t =
  List.fold_left enlargement_rect (List.hd lst) lst

let enlarge_rect_area r1 r2 = 
  let new_rect = enlargement_rect r1 r2
  in new_rect, area new_rect -. area r1

let enlarge_rect_peri r1 r2 = 
  let new_rect = enlargement_rect r1 r2
  in new_rect, perimeter new_rect -. perimeter r1

let to_json r =
  `Assoc [
    ("ll", r |> ll |> Point.to_json);
    ("ur", r |> ur |> Point.to_json);
  ]

let to_string (p1, p2) =
  "ll=" ^ Point.to_string p1 ^ "ur=" ^ Point.to_string p2
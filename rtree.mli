(** ['a t] is the type of R Trees whose elements have type ['a]. 
    The R Tree is mutable in the sense that the operations [add]
    and [remove] destructively modify the R Tree.
    AF: *)
type 'a t

(** [empty ()] is the empty R Tree. *)
val empty : 'a -> 'a t

(** [add x r] modifies [r] to include [x]. Requires [x] not in [r]. *)
val add : Point.t -> 'a -> 'a t -> unit

(** [remove x r] removes [x] from [r]. Leaves [r] unchanged if [x]
    not in [r]. *)
val remove : 'a -> 'a t -> unit

(** [union r1 r2] returns the set union of [r1] and [r2].
    [r1] contains less than or equal to elements than [r2].
    [r1] and [r2] are not allowed to contain duplicates *)
(* val union : 'a t -> 'a t -> 'a t *)

(** [inter r1 r2] returns the set intersection of [r1] and [r2].
    [r1] contains less than or equal to elements than [r2].	
    [r1] and [r2] are not allowed to contain duplicates *)
(* val inter : 'a t -> 'a t -> 'a t *)

(** [to_json t] is the JSON representation of [t]. See R Tree schema. *)
val to_json : 'a t -> Yojson.Basic.t

(*read and conversion operations*)
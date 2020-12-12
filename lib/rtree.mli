(** ['a t] is the type of R Trees whose elements have type ['a]. 
    The R Tree is mutable in the sense that the operations [add]
    and [remove] destructively modify the R Tree.
    AF: *)
type t

(** [empty ()] is the empty R Tree. *)
val empty : unit -> t

(** [new_tree p x] is a new R Tree with one element [x] at [p]. *)
val new_tree : Entry.t -> t

(** [add p x r] modifies [r] to include [x] at location [p]. Requires [x] not in
    [r]. *)
val add : Entry.t -> t -> unit

(** [remove x r] removes [x] from [r]. Leaves [r] unchanged if [x]
    not in [r]. *)
val remove : Entry.t -> t -> unit

(** [mem p x r] is [true] if [x] is in [r] at location [x]*)
val mem : Entry.t -> t -> bool

(** [find x t] traverses tree [t] for a given Entry [x] and returns the Entry 
    (leaf) that contains it. *)
val find : Entry.t -> t -> bool * t

(** [to_list r] is the list of Points of the tree *)
val to_list: t -> Entry.t list

(** [length r] is the tree length *)
val length: t -> int

(** [to_json t] is the JSON representation of [t]. See R Tree schema. *)
val to_json : t -> Yojson.Basic.t

(** TODO remove, this is for debugging the remove collapsing *)
val print_counters : int -> unit

val reset_counters : unit -> unit
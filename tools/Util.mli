val iter_rev : ('a -> unit) -> 'a list -> unit

val map_in_place : ('a -> 'a) -> 'a array -> unit
(** [map_in_place f arr] is the same as [List.map f arr] but modifies [arr] instead of returning a new array *)

val mapi_in_place : (int -> 'a -> 'a) -> 'a array -> unit
(** [mapi_in_place f arr] is the same as [List.mapi f arr] but modifies [arr] instead of returning a new array *)

val copy_in_place : 'a array -> 'a array -> unit
(** [copy_in_place arr model] copies the value of [model] in [arr]*)

val strg : float -> string
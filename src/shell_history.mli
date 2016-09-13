open Shell_types

type t

val empty : t

val add : uchar list -> t -> t

val up : uchar list -> t -> (uchar list * t) option

val down : uchar list -> t -> (uchar list * t) option

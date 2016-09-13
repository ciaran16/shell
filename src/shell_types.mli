
type uchar = int

type ustring = uchar array

type uchar_gen = unit -> uchar option

(** Returns a UTF-8 string. Skips invalid uchars. *)
val string_of_ustring : ustring -> string

(** Assumes the input string is UTF-8 encoded. Stops at malformed characters. *)
val ustring_of_string : string -> ustring

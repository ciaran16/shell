open Shell_types

type t

type prediction

val replacing_location : t -> int * int

(** The list of prediction values, sorted by their raw string value. *)
val to_list : t -> prediction list

val kind : t -> [`Names | `Values | `Invalid]

val doc_all : t -> string

val longest_prefix : t -> ustring option

val at_position : int -> Shell_ast.t -> t

val raw : prediction -> string

val doc : prediction -> string

val escape_to_ustring : prediction -> ustring

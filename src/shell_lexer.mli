open Shell_types

type t

type raw_token =
  | WORD of ustring
  | SEMICOLON
  | END

type token

val from_gen : uchar_gen -> t

val peek : t -> token
val next : t -> t
val is_terminated : t -> bool

val raw : token -> raw_token
val start_pos : token -> int
val end_pos : token -> int

val escape_word : ustring -> ustring

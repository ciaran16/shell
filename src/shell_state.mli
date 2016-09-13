open Shell_types

type t

val create : Shell_commands.Map.t -> t

val pos : t -> int

val move_left : t -> t

val move_right : t -> t

val insert_uchar : int -> t -> t

val backward_delete_uchar : t -> t

val forward_delete_uchar : t -> t

val input_is_terminated : t -> bool

val history_up : t -> t

val history_down : t -> t

val execute : print_error:(string -> unit Lwt.t) -> t ->
  (t * [`Exit | `Continue]) Lwt.t

val highlight : (ustring -> Shell_highlight.syntax -> 'a) -> t -> 'a list

val predictions : t -> Shell_predictions.t

val apply_predictions_longest_prefix : t -> t

val apply_prediction : Shell_predictions.prediction -> t -> t


(* Returns a thread that returns true if the shell should exit. *)
val eval : print_error:(string -> unit Lwt.t) -> Shell_ast.t ->
  [`Exit | `Continue] Lwt.t

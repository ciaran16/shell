
val run : ?prompt:(unit -> Notty.image) -> Shell_commands.Map.t -> unit Lwt.t

val print_error : string -> unit Lwt.t

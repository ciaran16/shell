open Shell_commands
open Shell_types

type t

type loc = int * int

type expr =
  | Empty of loc
  | Unknown_command of ustring * loc * arg list
  | Command of Command.full * ustring * loc * arg list
  | Statement of expr * loc * expr

and arg =
  | Unknown_opt of ustring * loc
  | Known_opt of Arg.Opt.t * ustring * loc * (ustring * loc) option
  | Unknown_pos_or_value of ustring * loc
  | Unknown_pos of ustring * loc
  | Known_pos of Arg.Pos.t * ustring * loc

val create : Shell_commands.Map.t -> t

val update : uchar list -> t -> t

val test : string -> t

val get_expr : t -> expr

val get_uchars : t -> uchar list

val get_commands : t -> Shell_commands.Map.t

val is_terminated : t -> bool

open Shell_types

type syntax = [
  | `White_space
  | `Operator
  | `Command_name
  | `Opt_name
  | `Opt_value
  | `Pos_value
  | `Unknown
]

val for_ast : Shell_ast.t -> f:(ustring -> syntax -> 'a) -> 'a list

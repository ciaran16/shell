open Shell_ast
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

type state = {
  uchars : uchar list;
  pos : int;
  acc : (syntax * ustring) list;
}

let highlight_loc syn (start_pos, end_pos) {uchars; pos; acc} =
  let rec split n l =
    if n <= 0 then [], l else
      match l with
      | [] -> [], []
      | hd::tl -> let ls, rs = split (n - 1) tl in (hd::ls, rs)
  in
  let aux n uchars =
    let ls, uchars = split n uchars in
    Array.of_list ls, uchars
  in
  let ws, uchars = aux (start_pos - pos) uchars in
  let us, uchars = aux (end_pos - start_pos) uchars in
  let acc =
    if Array.length ws = 0 then (syn, us) :: acc
    else (syn, us) :: (`White_space, ws) :: acc
  in
  {uchars; pos = end_pos; acc}

let highlight_arg arg state =
    match arg with
    | Unknown_opt (_, loc)
    | Unknown_pos_or_value (_, loc)
    | Unknown_pos (_, loc) -> state |> highlight_loc `Unknown loc
    | Known_opt (_, _, loc, None) -> state |> highlight_loc `Opt_name loc
    | Known_pos (_, _, loc) -> state |> highlight_loc `Pos_value loc
    | Known_opt (_, _, loc, Some (_, v_loc)) ->
      state |> highlight_loc `Opt_name loc |> highlight_loc `Opt_value v_loc

let rec highlight_args args state =
  match args with
  | [] -> state
  | hd::tl -> state |> highlight_arg hd |> highlight_args tl

let rec highlight_expr e state =
  match e with
  | Empty loc -> state |> highlight_loc `White_space loc
  | Statement (left, loc, right) ->
    state |> highlight_expr left
    |> highlight_loc `Operator loc
    |> highlight_expr right
  | Unknown_command (_, loc, args) ->
    state |> highlight_loc `Unknown loc |> highlight_args args
  | Command (_, _, loc, args) ->
    state |> highlight_loc `Command_name loc |> highlight_args args

let for_ast ast ~f =
  let e = get_expr ast in
  let uchars = get_uchars ast in
  let {acc; uchars; _} = highlight_expr e {uchars; pos = 0; acc = []} in
  let acc = (`White_space, Array.of_list uchars) :: acc in
  List.rev_map (fun (syn, ustring) -> f ustring syn) acc

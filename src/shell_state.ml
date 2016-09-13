open Shell_types
module Ast = Shell_ast
module Predictions = Shell_predictions
module History = Shell_history

type t = {
  left : uchar list;
  right : uchar list;
  pos : int;
  history : History.t;
  full : uchar list Lazy.t;
  ast : Ast.t Lazy.t;
  predictions : Predictions.t Lazy.t;
}

let create commands =
  let ast = Ast.create commands in {
    (* Reverse ordered, so (List.rev left) @ right gives the whole input. *)
    left = [];
    right = [];
    full = lazy ([]);
    pos = 0;
    ast = lazy (ast);
    predictions = lazy (ast |> Predictions.at_position 0);
    history = History.empty;
  }

let pos {pos; _} = pos

let move_left ({left; right; pos; _} as t) =
  match left with
  | [] -> t
  | hd::tl -> {t with left = tl; right = hd::right; pos = pos - 1}

let move_right ({left; right; pos; _} as t) =
  match right with
  | [] -> t
  | hd::tl -> {t with left = hd::left; right = tl; pos = pos + 1}

let fix ({left; right; ast; pos; _} as t) =
  let full = lazy (List.rev_append left right) in
  let ast = lazy (Lazy.force ast |> Ast.update (Lazy.force full)) in
  let predictions = lazy (Lazy.force ast |> Predictions.at_position pos) in
  {t with full; ast; predictions}

let insert_uchar u ({left; pos; _} as t) =
  {t with left = u::left; pos = pos + 1} |> fix

let forward_delete_uchar ({right; _} as t) =
  match right with
  | [] -> t
  | _::tl -> {t with right = tl} |> fix

let backward_delete_uchar ({left; pos; _} as t) =
  match left with
  | [] -> t
  | _::tl -> {t with left = tl; pos = pos - 1} |> fix

let input_is_terminated {ast; _} = Ast.is_terminated (Lazy.force ast)

let modify_history history_f ({history; full; _} as t) =
  match history |> history_f (Lazy.force full) with
  | None -> t
  | Some (l, history) -> {
      t with
      left = List.rev l;
      right = [];
      pos = List.length l;
      history
    } |> fix

let history_up t = t |> modify_history History.up

let history_down t = t |> modify_history History.down

let execute ~print_error {full; ast; history; _} =
  let open Lwt.Infix in
  let ast = Lazy.force ast in
  Shell_eval.eval ~print_error ast >|= fun should_exit ->
  let state = Ast.get_commands ast |> create in
  let history = history |> History.add (Lazy.force full) in
  {state with history}, should_exit

let highlight f {ast; _} = Shell_highlight.for_ast (Lazy.force ast) ~f

let predictions {predictions; _} = Lazy.force predictions

let apply_prediction_us us ({left; right; pos; predictions; _} as t) =
let rec drop n = function
  | [] -> []
  | _::tl as l -> if n <= 0 then l else drop (n - 1) tl
in
let predictions = Lazy.force predictions in
let start_pos, end_pos = Predictions.replacing_location predictions in
let left = left |> drop (pos - start_pos) in
let left = us |> Array.fold_left (fun l u -> u::l) left in
let right = right |> drop (end_pos - pos) in
{t with left; right; pos = start_pos + Array.length us} |> fix

let apply_predictions_longest_prefix ({predictions; _} as t) =
  match Predictions.longest_prefix (Lazy.force predictions) with
  | None -> t
  | Some us -> t |> apply_prediction_us us

let apply_prediction prediction t =
  t |> apply_prediction_us (Predictions.escape_to_ustring prediction)

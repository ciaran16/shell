open Shell_commands
open Shell_ast
open Shell_types

type prediction = {
  raw : string;
  doc : string;
  process : (string -> string) option
}

let raw {raw; _} = raw

let doc {doc; _} = doc

let escape_to_ustring {raw; process; _} =
  let raw = match process with Some f -> f raw | None -> raw in
  let us = ustring_of_string raw |> Shell_lexer.escape_word in
  let len = Array.length us in
  if len >= 2 && us.(0) = 0x22 && us.(len - 1) = 0x22 (* "\"" *) then
    Array.sub us 0 (len - 1)
  else us

type t = {
  predictions : prediction list;
  kind : [`Names | `Values | `Invalid];
  doc_all : string;
  loc : int * int;
}

let to_list {predictions; _} = predictions

let kind {kind; _} = kind

let doc_all {doc_all; _} = doc_all

let replacing_location {loc; _} = loc

let longest_prefix t =
  (* The predictions are in alphabetical order, so just need to find the
     longest prefix of the first and last predictions. *)
  match t |> to_list with
  | [] -> None
  | a::tl ->
    match List.rev tl with
    | [] -> Some (escape_to_ustring a)
    | b::_ ->
      let rec aux a b i max =
        if i = max || a.(i) <> b.(i) then []
        else a.(i) :: aux a b (i + 1) max
      in
      let a = escape_to_ustring a in
      let b = escape_to_ustring b in
      let max = min (Array.length a) (Array.length b) in
      Some (aux a b 0 max |> Array.of_list)

let make_invalid ~loc = {
  predictions = [];
  kind = `Invalid;
  doc_all = "";
  loc;
}

(* Note that this returns false in the case when the string is equal to
   the prefix. This is intended. *)
let has_prefix prefix s =
  let len = String.length prefix in
  String.length s > len && String.sub s 0 len = prefix

let filter_bindings prefix_us l =
  if Array.length prefix_us = 0 then l
  else
    let prefix = string_of_ustring prefix_us in
    l |> List.filter (fun (k, _) -> k |> has_prefix prefix)

let predict_name ?process prefix ~loc ~bindings ~doc_f =
  let predictions =
    bindings |> filter_bindings prefix |>
    List.map (fun (raw, v) -> {raw; doc = doc_f v; process})
  in
  {predictions; kind = `Names; doc_all = "Possible names."; loc}

let predict_opt_name name ~loc command =
  let process name =
    (if String.length name = 1 then "-" else "--") ^ name
  in
  let bindings = command |> Command.opt_bindings in
  let doc_f opt = Arg.doc (`Opt opt) in
  predict_name name ~loc ~bindings ~doc_f ~process

let predict_command name ~loc commands =
  let bindings = commands |> Map.bindings in
  predict_name name ~loc ~bindings ~doc_f:Command.doc

let predict_value value ~loc arg =
  let doc = Arg.doc arg in
  let predictions =
    arg |> Arg.predict (string_of_ustring value) |>
    List.filter (has_prefix @@ string_of_ustring value) |>
    List.sort String.compare |>
    List.map (fun raw -> {raw; doc; process = None})
  in
  {predictions; kind = `Values; doc_all = doc; loc}

let predict_opt_value value ~loc opt =
  `Opt opt |> predict_value value ~loc

let predict_pos_value value ~loc pos =
  `Pos pos |> predict_value value ~loc

let predict_unknown i ~loc command =
  match command |> Command.lookup_pos i with
  | None -> command |> predict_opt_name [||] ~loc
  | Some pos_arg -> pos_arg |> predict_pos_value [||] ~loc

let is_before (start_pos, _) pos = pos < start_pos

let is_after (_, end_pos) pos = pos > end_pos

let rec predict_arg command pos i args =
  let default () = command |> predict_unknown i ~loc:(pos, pos) in
  let when_is_in loc ~do_f ?(incr=false) ~tl pos =
    if pos |> is_before loc then default ()
    else if pos |> is_after loc then
      predict_arg command pos (if incr then i + 1 else i) tl
    else do_f ()
  in
  match args with
  | [] -> default ()
  | Unknown_pos_or_value (_, loc) :: tl
  | Unknown_pos (_, loc) :: tl ->
    pos |> when_is_in loc ~do_f:(fun () ->
        make_invalid ~loc
      ) ~incr:true ~tl
  | Unknown_opt (name, loc) :: tl ->
    pos |> when_is_in loc ~do_f:(fun () ->
        command |> predict_opt_name name ~loc
      ) ~tl
  | Known_pos (pos_arg, value, loc) :: tl ->
    pos |> when_is_in loc ~do_f:(fun () ->
        pos_arg |> predict_pos_value value ~loc
      ) ~incr:true ~tl
  | Known_opt (opt, name, loc, value_o) :: tl ->
    if pos |> is_before loc then default ()
    else if pos |> is_after loc then begin
      match value_o with
      | None ->
        if Arg.Opt.is_flag opt then default ()
        else opt |> predict_opt_value [||] ~loc:(pos, pos)
      | Some (value, value_loc) ->
        if pos |> is_before value_loc then
          opt |> predict_opt_value [||] ~loc
        else if pos |> is_after value_loc then predict_arg command pos i tl
        else opt |> predict_opt_value value ~loc
    end else command |> predict_opt_name name ~loc

let rec predict_expr commands pos e =
  let default () = commands |> predict_command [||] ~loc:(pos, pos) in
  match e with
  | Empty _ -> default ()
  | Statement (left, loc, right) ->
    if pos |> is_before loc then predict_expr commands pos left
    else if pos |> is_after loc then predict_expr commands pos right
    else default ()
  | Unknown_command (name, loc, _) ->
    if pos |> is_before loc then default ()
    else if pos |> is_after loc then make_invalid ~loc
    else commands |> predict_command name ~loc
  | Command (command, name, loc, args) ->
    if pos |> is_before loc then default ()
    else if pos |> is_after loc then predict_arg command pos 0 args
    else commands |> predict_command name ~loc

let at_position pos ast =
  let e = get_expr ast in
  let commands = get_commands ast in
  predict_expr commands pos e

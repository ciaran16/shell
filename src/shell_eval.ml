open Shell_commands
open Shell_ast
open Shell_types
open Lwt.Infix
open Result

type info = {
  commands : Map.t;
  print_error : string -> unit Lwt.t
}

let print_error {print_error; _} s = print_error s

let process_args args =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | Known_pos (pos, us, _)::tl ->
      aux ((`Pos pos, string_of_ustring us) :: acc) tl
    | Known_opt (opt, _, _, value_o)::tl ->
      let opt =
        match value_o with
        | None -> (`Opt opt, "")
        | Some (us, _) -> (`Opt opt, string_of_ustring us)
      in
      aux (opt::acc) tl
    | _ -> Error "Unknown arg."
  in
  aux [] args

let rec eval_expr info = function
  | Empty _ -> Lwt.return `Continue
  | Statement (left, _, right) ->
    begin
      eval_expr info left >>= function
      | `Exit -> Lwt.return `Exit
      | `Continue -> eval_expr info right
    end
  | Unknown_command (us, _, _) ->
    "Unknown command '" ^ string_of_ustring us ^ "'." |>
    print_error info >|= fun () -> `Continue
  | Command (command, _, _, args) ->
    begin
      match process_args args with
      | Error msg -> msg |> print_error info >|= fun () -> `Continue
      | Ok processed ->
        let execute () = command |> Command.eval processed in
        let handle_exn exn = Printexc.to_string exn |> print_error info in
        Lwt.catch execute handle_exn >|= fun () ->
        if Command.should_exit command then `Exit else `Continue
    end

let eval ~print_error ast =
  if not (Shell_ast.is_terminated ast) then
    "Input isn't terminated." |> print_error >|= fun () -> `Continue
  else
    let commands = Shell_ast.get_commands ast in
    let e = Shell_ast.get_expr ast in
    eval_expr {commands; print_error} e

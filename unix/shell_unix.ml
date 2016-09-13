open Notty
open Lwt.Infix
module St = Shell_state
module Predictions = Shell_predictions

type action =
  | Execute
  | Complete
  | Apply of (St.t -> St.t)

let print_error s =
  I.(string A.(st bold ++ fg red) "Error: " <|> string A.empty s) |>
  Notty_lwt.output_image_endline ~fd:Lwt_unix.stderr

module Terminal = struct
  let esc s = "\x1b[" ^ s |> print_string

  let up () = esc "A"

  let delete_line () = esc "2K"; esc "G"

  let to_column n =
    let n = if n < 0 then 0 else n in
    esc (string_of_int n ^ "G")

  let width () =
    match Notty_lwt.winsize Lwt_unix.stdin with
    | None -> 80
    | Some (w, _) -> w
end

let highlighter us syn =
  let attr =
    let open A in
    match syn with
    | `Command_name -> fg blue
    | `Opt_name | `Opt_value -> fg green
    | `Pos_value -> empty
    | `Operator -> fg green
    | `Unknown -> fg red ++ st bold
    | `White_space -> fg yellow
  in
  I.uchars attr us

let image_height = ref 0

let draw ~prompt_f state =
  let wrap width image =
    let rec go off = I.hcrop off 0 image ::
      if I.width image - off > width then go (off + width) else []
    in go 0 |> I.vcat |> I.hsnap ~align:`Left width
  in
  let create_image ~width ~prompt_f state =
    let prompt =
      let w = width / 2 in
      let image = I.(prompt_f () <|> string A.empty "> ") in
      if I.width image <= w then image
      else I.(string A.empty "..." <|> hsnap ~align:`Right (w - 3) image)
    in
    let image = state |> St.highlight highlighter |> I.hcat in
    I.(prompt <|> image), I.width prompt
  in
  let output image =
    Terminal.delete_line ();
    for _ = 1 to !image_height - 1 do
      Terminal.up ();
      Terminal.delete_line ();
    done;
    Notty_unix.output_image image;
    image_height := I.height image
  in
  let width = Terminal.width () in
  let image, offset = create_image ~width ~prompt_f state in
  wrap width image |> output;
  let pos = St.pos state + offset + 1 in
  Terminal.to_column pos;
  flush stdout;
  Lwt.return_unit

let is_executing = ref false

let perform_action action state =
  match action with
  | Execute ->
    (* TODO
       if not (St.input_is_terminated state) then state |> St.insert_uchar 0xA
    *)
    is_executing := true;
    image_height := 0;
    Notty_lwt.output_image_endline I.empty >>= fun () ->
    St.execute state ~print_error >>= fun (state, continue) ->
    Lwt_io.flush Lwt_io.stdout >|= fun () ->
    is_executing := false;
    (state, continue)
  | Apply f -> (f state, `Continue) |> Lwt.return
  | Complete ->
    (St.apply_predictions_longest_prefix state, `Continue) |> Lwt.return

let rec handle_actions actions_stream ~prompt_f state =
  let open Lwt_stream in
  is_empty actions_stream >>= function
  | true -> Lwt.return_unit
  | false ->
    let rec aux state actions =
      match actions with
      | [] -> (state, `Continue) |> Lwt.return
      | action::actions ->
        state |> perform_action action >>= function
        | state, `Exit -> (state, `Exit) |> Lwt.return
        | state, `Continue -> aux state actions
    in
    actions_stream |> get_available |> aux state >>= function
    | _, `Exit -> Lwt.return_unit
    | state, `Continue ->
      state |> draw ~prompt_f >>= fun () ->
      state |> handle_actions actions_stream ~prompt_f

let reset_f = ref None

let reset () =
  match !reset_f with
  | None -> Lwt.return_unit
  | Some f -> reset_f := None; f ()

let () = Lwt_main.at_exit reset

let action_for_event : Notty.Unescape.event -> action option = function
  | `Key (`Enter, []) -> Some (Execute)
  | `Key (`Uchar u, []) -> Some (Apply (St.insert_uchar u))
  | `Key (`Backspace, []) -> Some (Apply St.backward_delete_uchar)
  | `Key (`Delete, []) -> Some (Apply St.forward_delete_uchar)
  | `Key (`Arrow `Up, []) -> Some (Apply St.history_up)
  | `Key (`Arrow `Down, []) -> Some (Apply St.history_down)
  | `Key (`Arrow `Left, []) -> Some (Apply St.move_left)
  | `Key (`Arrow `Right, []) -> Some (Apply St.move_right)
  | `Key (`Tab, []) -> Some (Complete)
  | `Key _ -> None
  | `Mouse _ -> None

let create_action_stream () =
  let open Lwt_unix in
  (* Disabling canonical mode. *)
  tcgetattr stdin >>= fun tc ->
  reset_f := Some (fun () -> tcsetattr stdin TCSAFLUSH tc);
  let new_tc = {tc with c_icanon = false; c_echo = false} in
  tcsetattr stdin TCSAFLUSH new_tc >|= fun () ->
  (* Getting events from stdin. *)
  let filter = Unescape.create () in
  let buffer = Bytes.create 1024 in
  let rec get_event () =
    match Unescape.next filter with
    | `End -> Lwt.return_none
    | #Unescape.event as event ->
      if !is_executing then get_event ()
      else Lwt.return_some event
    | `Await ->
      read stdin buffer 0 1024 >>= fun n ->
      Unescape.input filter buffer 0 n;
      get_event ()
  in
  let rec get_action () =
    get_event () >>= function
    | None -> Lwt.return_none
    | Some event ->
      match action_for_event event with
      | None -> get_action ()
      | Some action -> Lwt.return_some action
  in
  Lwt_stream.from get_action

let run_lock = Lwt_mutex.create ()

let run ?prompt commands =
  Lwt_mutex.with_lock run_lock (fun () ->
      let prompt_f =
        match prompt with
        | None -> fun () -> I.empty
        | Some f -> f
      in
      create_action_stream () >>= fun actions ->
      let state = St.create commands in
      state |> draw ~prompt_f >>= fun () ->
      state |> handle_actions actions ~prompt_f >>=
      reset
    )

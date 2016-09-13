open Shell_commands
open Shell_types
module L = Shell_lexer

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

let raw (lexer, _) = L.peek lexer |> L.raw
let loc (lexer, _) = let tok = L.peek lexer in (L.start_pos tok, L.end_pos tok)
let commands (_, commands) = commands
let next (lexer, commands) = (L.next lexer, commands)

let (>>=) (value, state) f = (f value, state)

let dash = 0x2d

let sub_ustring us start = Array.sub us start (Array.length us - start)

let is_long_name w = Array.length w >= 2 && w.(0) = dash && w.(1) = dash

let is_short_name w =
  Array.length w >= 1 && w.(0) = dash &&
  (Array.length w = 1 || w.(1) <> dash)

let get_args w loc =
  if is_long_name w then [Unknown_opt (sub_ustring w 2, loc)]
  else if is_short_name w then
    [Unknown_opt (sub_ustring w 1, loc)]
  else [Unknown_pos_or_value (w, loc)]

let rec parse_args state =
  match raw state with
  | L.END | L.SEMICOLON -> [], state
  | L.WORD w ->
    parse_args (next state) >>= fun args ->
    get_args w (loc state) @ args

let rec lookup_args i command = function
  | [] -> []
  | Unknown_pos (us, loc)::tl | Unknown_pos_or_value (us, loc)::tl ->
    let hd =
      match command |> Command.lookup_pos i with
      | None -> Unknown_pos (us, loc)
      | Some pos -> Known_pos (pos, us, loc)
    in hd :: lookup_args (i + 1) command tl
  | (Unknown_opt (us, loc) as hd)::tl ->
    begin
      match command |> Command.lookup_opt (string_of_ustring us) with
      | None -> hd :: lookup_args i command tl
      | Some opt ->
        let value_o, tl =
          if Arg.Opt.is_flag opt then None, tl
          else
            match tl with
            | Unknown_pos_or_value (us, loc) :: tl -> Some (us, loc), tl
            | _ -> None, tl
        in
        Known_opt (opt, us, loc, value_o) :: lookup_args i command tl
    end
  | hd::tl -> hd :: lookup_args i command tl

let parse_command name loc state =
  parse_args state >>= fun args ->
  match commands state |> Map.get (string_of_ustring name) with
  | None -> Unknown_command (name, loc, args)
  | Some c -> Command (c, name, loc, lookup_args 0 c args)

let rec parse_expr state =
  match raw state with
  | L.END | L.SEMICOLON ->
    let pos, _ = loc state in parse_op (Empty (pos, pos)) state
  | L.WORD w ->
    let left, state = parse_command w (loc state) (next state) in
    parse_op left state

and parse_op left state =
  match raw state with
  | L.END -> left, state
  | L.SEMICOLON ->
    parse_expr (next state) >>= fun right ->
    Statement (left, loc state, right)
  | L.WORD _ -> assert false

type t = {
  e : expr;
  uchars : uchar list;
  terminated : bool;
  commands : Map.t;
}

let create commands = {
  e = Empty (0, 0);
  uchars = [];
  terminated = true;
  commands;
}

let create_gen l =
  let r = ref l in
  fun () ->
    match !r with
    | [] -> None
    | hd::tl -> r := tl; Some hd

let update uchars {commands; _} =
  let lexer = L.from_gen (create_gen uchars) in
  let e, (lexer, _) = parse_expr (lexer, commands) in
  let terminated = L.is_terminated lexer in
  {e; uchars; terminated; commands}

let test s =
  let decoder = Uutf.decoder (`String s) in
  let rec aux () =
    match Uutf.decode decoder with
    | `Await | `Malformed _ -> failwith ""
    | `End -> []
    | `Uchar u -> u :: aux ()
  in
  let t = create (Map.empty) in
  update (aux ()) t

let get_expr {e; _} = e
let get_uchars {uchars; _} = uchars
let get_commands {commands; _} = commands
let is_terminated {terminated; _} = terminated

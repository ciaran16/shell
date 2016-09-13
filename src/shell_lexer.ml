open Shell_types

type raw_token =
  | WORD of ustring
  | SEMICOLON
  | END

let rev_array_concat l = List.rev l |> Array.concat

let lexeme = Sedlexing.lexeme
let sub_lexeme = Sedlexing.sub_lexeme
let get_pos = Sedlexing.lexeme_end

let special = [%sedlex.regexp? ';']
let bare_other = [%sedlex.regexp? white_space | '"' | '\\' | special]
let string_other = [%sedlex.regexp? '"' | '\\']

let rec lex_bare buf acc =
  match%sedlex buf with
  | '"' -> lex_string buf acc
  | '\\', eof -> WORD (rev_array_concat acc), false
  | '\\', any -> lex_bare buf (sub_lexeme buf 1 2 :: acc)
  | Plus (Compl bare_other) -> lex_bare buf (lexeme buf :: acc)
  | _ (* eof | white_space | special *) -> WORD (rev_array_concat acc), true

and lex_string buf acc =
  match%sedlex buf with
  | '"' -> lex_bare buf acc
  | '\\', '"' -> lex_string buf @@ sub_lexeme buf 1 2 :: acc
  | '\\', any -> lex_string buf @@ lexeme buf :: acc
  | Plus (Compl string_other) -> lex_string buf @@ lexeme buf :: acc
  | _ (* eof *) -> WORD (rev_array_concat acc), false

type token = raw_token * int * int

let rec lex buf =
  let start_pos = get_pos buf in
  let emit raw = (raw, start_pos, get_pos buf), true in
  let emit_word lex_word =
    let word, term = lex_word buf [] in
    (word, start_pos, get_pos buf), term
  in
  match%sedlex buf with
  | Plus (white_space | "\\\n") -> lex buf
  | eof -> emit END
  | ';' -> emit SEMICOLON
  | special -> assert false
  | '"' -> emit_word lex_string
  | _ -> emit_word lex_bare

type t = {
  buf : Sedlexing.lexbuf;
  curr : token;
  terminated : bool;
}

let from_gen gen =
  let buf = Sedlexing.from_gen gen in
  let curr, terminated = lex buf in
  {buf; curr; terminated}

let peek {curr; _} = curr

let next ({buf; terminated; _} as t) =
  let curr, term = lex buf in
  {t with curr; terminated = terminated && term}

let is_terminated {terminated; _} = terminated

let raw (raw, _, _) = raw
let start_pos (_, start_pos, _) = start_pos
let end_pos (_, _, end_pos) = end_pos

let escape_string us =
  let backslash = 0x5C in
  let double_quote = 0x22 in
  let rec aux buf acc =
    match%sedlex buf with
    | string_other -> lexeme buf :: [|backslash|] :: acc |> aux buf
    | Plus (Compl string_other) -> lexeme buf :: acc |> aux buf
    | _ (* eof *) -> [|double_quote|] :: acc |> rev_array_concat
  in
  [|double_quote|] :: [] |> aux (Sedlexing.from_int_array us)

let escape_word us =
  let rec should_string buf =
    match%sedlex buf with
    | bare_other -> true
    | any -> should_string buf
    | _ (* eof *) -> false
  in
  if should_string (Sedlexing.from_int_array us) then escape_string us else us

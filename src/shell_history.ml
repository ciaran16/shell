open Shell_types

type entry = string

type t = {
  up : entry list;
  down : entry list;
}

let wrap l = Array.of_list l |> string_of_ustring

let unwrap entry = ustring_of_string entry |> Array.to_list

let empty = {
  up = [];
  down = [];
}

let add current {up; down} =
  let up = wrap current :: List.rev_append down up in
  {up; down = []}

let up current {up; down} =
  match up with
  | [] -> None
  | hd::tl -> Some (unwrap hd, {up = tl; down = wrap current :: down})

let down current {up; down} =
  match down with
  | [] -> None
  | hd::tl -> Some (unwrap hd, {up = wrap current :: up; down = tl})

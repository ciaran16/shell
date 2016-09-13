type uchar = int

type ustring = uchar array

type uchar_gen = unit -> uchar option

let string_of_ustring ustring =
  let buffer = Buffer.create (Array.length ustring) in
  let add u = if Uutf.is_uchar u then Uutf.Buffer.add_utf_8 buffer u in
  ustring |> Array.iter add;
  Buffer.contents buffer

let ustring_of_string s =
  let rec aux decoder =
    match Uutf.decode decoder with
    | `End -> []
    | `Uchar u -> u :: aux decoder
    | `Malformed _ -> []
    | `Await -> []
  in
  `String s |> Uutf.decoder ~encoding:`UTF_8 |> aux |> Array.of_list

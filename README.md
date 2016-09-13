## Shell (In development)

Lets you create interactive shells in OCaml.

## Example

```ocaml
open Shell.Commands

let echo_cmd =
  let echo = Lwt_io.printl in
  let arg = Arg.pos ~predict:(fun _ -> ["Hello world"]) () in
  Command.(create "echo" echo $ arg)

let commands = Map.empty |> Map.add echo_cmd

let () =
  Lwt_main.run (Shell_unix.run commands)
```

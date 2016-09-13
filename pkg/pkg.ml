#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let unix = Conf.with_pkg "unix" ~default:true

let () =
  Pkg.describe "shell" @@ fun c ->
  let unix = Conf.value c unix in
  Ok [
    Pkg.mllib "src/shell.mllib";
    Pkg.mllib ~cond:unix "unix/shell_unix.mllib";
  ]

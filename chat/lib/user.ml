open! Base
open! Core
open! Async

type t = string

let random_name () =
  let i = Random.int 100 in
  "User_" ^ Int.to_string i
;;

let arg () =
  let%map_open.Command username =
    flag
      "username"
      (optional_with_default (random_name ()) string)
      ~doc:"USER Your name in the chat"
  in
  username
;;

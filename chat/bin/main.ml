open! Base
open! Core
open! Async
open Chat

let server_command =
  Command.async
    ~summary:"Running the server"
    (let%map_open.Command () = Log.Global.set_level_via_param () in
     fun () ->
       Log.Global.info_s [%message "Starting the server"];
       Log.Global.debug_s [%message "Debug enabled"];
       Server.start ~port:80)
;;

let client_command =
  Command.async
    ~summary:"Running the client"
    (let%map_open.Command () = Log.Global.set_level_via_param ()
     and username = User.arg () in
     fun () ->
       Log.Global.info_s [%message "Starting the client"];
       Log.Global.debug_s [%message "Debug enabled"];
       Client.start ~host:"localhost" ~port:80 ~username)
;;

let main_command =
  Command.group
    ~summary:"Simple chat engine"
    [ "server", server_command; "client", client_command ]
;;

let () = Command_unix.run ~version:"1.0" main_command

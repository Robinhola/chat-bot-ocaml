open! Base
open! Core
open! Async
open Chat

let hostname () =
  let%map_open.Command hostname =
    flag
      "hostname"
      (optional_with_default "localhost" string)
      ~doc:"HOST The hostname to connect to (defaults to localhost)"
  in
  hostname
;;

let port () =
  let%map_open.Command port =
    flag
      "port"
      (optional_with_default 470 int)
      ~doc:"PORT The port to connect to (defaults to 470)"
  in
  port
;;

let server_command =
  Command.async
    ~summary:"Running the server"
    (let%map_open.Command () = Log.Global.set_level_via_param ()
     and port = port () in
     fun () ->
       Log.Global.info_s [%message "Starting the server"];
       Log.Global.debug_s [%message "Debug enabled"];
       Server.start ~port)
;;

let client_command =
  Command.async
    ~summary:"Running the client"
    (let%map_open.Command () = Log.Global.set_level_via_param ()
     and host = hostname ()
     and port = port ()
     and username = User.arg () in
     fun () ->
       Log.Global.info_s [%message "Starting the client"];
       Log.Global.debug_s [%message "Debug enabled"];
       Client.start ~host ~port ~username)
;;

let main_command =
  Command.group
    ~summary:"Simple chat engine"
    [ "server", server_command; "client", client_command ]
;;

let () = Command_unix.run ~version:"1.0" main_command

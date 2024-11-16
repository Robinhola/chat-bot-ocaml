open! Base
open! Core
open! Async

let user_joined ~username writer =
  let msg = username ^ " joined the chat!" in
  Pipe.write_if_open writer msg
;;

let user_left ~username writer =
  let msg = username ^ " is leaving, bye!" in
  Pipe.write_if_open writer msg
;;

let print_help () =
  print_endline "--- Help --- ";
  print_endline "Use \\bye to say goodbye and leave the chat!";
  print_endline "------------"
;;

let start ~host ~port ~username =
  let stdin = Reader.stdin |> Lazy.force |> Reader.pipe in
  let host_and_port = Host_and_port.create ~host ~port in
  Tcp.with_connection
    ~timeout:(Time_float.Span.of_sec 5.)
    (Tcp.Where_to_connect.of_host_and_port host_and_port)
    (fun _socket reader writer ->
       let ws = Websocket.create ~role:Client reader writer in
       let wsreader, wswriter = Websocket.pipes ws in
       (* Unused because we always want to stay subscribed to the server *)
       let _bus =
         let receiver = Websocket.frame_received ws in
         Bus.subscribe_exn receiver [%here] ~f:(fun opcode ->
           Opcode.log opcode;
           match opcode with
           | Text ->
             Log.Global.debug_s [%message "Reading..."];
             don't_wait_for
               (match%map Pipe.read wsreader with
                | `Eof -> Log.Global.info_s [%message "Bye!"]
                | `Ok msg ->
                  Log.Global.debug_s [%message "Read" (msg : string)];
                  print_endline msg)
           | _ -> ())
       in
       let%bind () = user_joined ~username wswriter in
       let rec loop () =
         let%bind () = Clock.after (Time_float_unix.Span.of_ms 100.) in
         match Pipe.is_closed wswriter, Pipe.is_empty stdin with
         | false, false ->
           Log.Global.debug_s [%message "Still connected"];
           (match%bind Pipe.read stdin with
            | `Eof ->
              Log.Global.info_s [%message "User closed stdin"];
              user_left ~username wswriter
            | `Ok "\\bye\n" -> user_left ~username wswriter
            | `Ok "\\help\n" ->
              print_help ();
              loop ()
            | `Ok "" -> loop ()
            | `Ok line ->
              let line = String.strip line in
              Log.Global.debug_s [%message "Received from user" (line : string)];
              let%bind () = Pipe.write_if_open wswriter (username ^ ": " ^ line) in
              loop ())
         | false, true -> loop ()
         | true, _ ->
           Log.Global.error_s [%message "Lost connection"];
           Deferred.unit
       in
       loop ())
;;

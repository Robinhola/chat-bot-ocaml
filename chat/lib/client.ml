open! Base
open! Core
open! Async

let random_name () =
  let i = Random.int 100 in
  "User_" ^ Int.to_string i
;;

let start ~host ~port =
  let stdin = Reader.stdin |> Lazy.force |> Reader.pipe in
  let name = random_name () in
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
                | `Ok msg -> Log.Global.info_s [%message "Read" (msg : string)])
           | _ -> ())
       in
       let rec echo_loop () =
         let%bind () = Clock.after (Time_float_unix.Span.of_ms 100.) in
         match Pipe.is_closed wswriter, Pipe.is_empty stdin with
         | false, false ->
           Log.Global.debug_s [%message "Still connected"];
           (match%bind Pipe.read stdin with
            | `Eof ->
              Log.Global.info_s [%message "User closed stdin"];
              Deferred.unit
            | `Ok "" -> echo_loop ()
            | `Ok line ->
              let line = String.strip line in
              Log.Global.debug_s [%message "Received from user" (line : string)];
              let%bind () = Pipe.write_if_open wswriter (name ^ ": " ^ line) in
              echo_loop ())
         | false, true -> echo_loop ()
         | true, _ ->
           Log.Global.error_s [%message "Lost connection"];
           Deferred.unit
       in
       echo_loop ())
;;

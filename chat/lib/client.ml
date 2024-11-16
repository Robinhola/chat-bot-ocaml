open! Base
open! Core
open! Async

let read_opcode (opcode : Websocket.Opcode.t) =
  match opcode with
  | Continuation -> Log.Global.debug_s [%message "Continuation"]
  | Text -> Log.Global.debug_s [%message "Text"]
  | Binary -> Log.Global.debug_s [%message "Binary"]
  | Close -> Log.Global.debug_s [%message "Close"]
  | Ping -> Log.Global.debug_s [%message "Ping"]
  | Pong -> Log.Global.debug_s [%message "Pong"]
  | _ -> Log.Global.error_s [%message "Unused"]
;;

let start ~host ~port =
  let name =
    let i = Random.int 100 in
    "User_" ^ Int.to_string i
  in
  let host_and_port = Host_and_port.create ~host ~port in
  Tcp.with_connection
    ~timeout:(Time_float.Span.of_sec 5.)
    (Tcp.Where_to_connect.of_host_and_port host_and_port)
    (fun _socket reader writer ->
       let ws = Websocket.create ~role:Client reader writer in
       let wsreader, wswriter = Websocket.pipes ws in
       let receiver = Websocket.frame_received ws in
       (* Unused because we always want to stay subscribed to the server *)
       let _bus =
         Bus.subscribe_exn receiver [%here] ~f:(fun opcode ->
           Log.Global.debug_s [%message "Sub" (opcode : Websocket.Opcode.t)];
           let () = read_opcode opcode in
           don't_wait_for
             (Log.Global.debug_s [%message "Reading..."];
              match%map Pipe.read wsreader with
              | `Eof -> Log.Global.debug_s [%message "Bye!"]
              | `Ok msg -> Log.Global.info_s [%message "Read" (msg : string)]))
       in
       let stdin = Reader.stdin |> Lazy.force in
       let rec echo_loop () =
         let%bind () = Clock.after (Time_float_unix.Span.of_ms 500.) in
         match Pipe.is_closed wsreader with
         | false ->
           let%bind () =
             Log.Global.debug_s [%message "Still connected"];
             match%bind
               Reader.really_read_line ~wait_time:(Time_float_unix.Span.of_ms 200.) stdin
             with
             | None ->
               Log.Global.debug_s [%message "No input, skip"];
               Deferred.unit
             | Some line ->
               Log.Global.debug_s [%message "received from user" (line : string)];
               Pipe.write_if_open wswriter (name ^ ": " ^ line)
           in
           echo_loop ()
         | true ->
           Log.Global.debug_s [%message "Lost connection"];
           Deferred.unit
       in
       echo_loop ())
;;

open! Base
open! Core
open! Async

let start ~host ~port =
  let host_and_port = Host_and_port.create ~host ~port in
  Tcp.with_connection
    ~timeout:(Time_float.Span.of_sec 5.)
    (Tcp.Where_to_connect.of_host_and_port host_and_port)
    (fun _socket reader writer ->
       let ws = Websocket.create ~role:Client reader writer in
       let receiver = Websocket.frame_received ws in
       let _bus =
         Bus.subscribe_exn receiver [%here] ~f:(fun opcode ->
           Log.Global.debug_s [%message "Sub" (opcode : Websocket.Opcode.t)])
       in
       let rec echo_loop () =
         Log.Global.debug_s [%message "Send ping"];
         (* Websocket.send_ping ws "Hello"; *)
         let%bind () = Clock.after (Time_float_unix.Span.of_ms 1000.) in
         echo_loop ()
       in
       echo_loop ())
;;

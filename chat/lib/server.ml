open! Base
open! Core
open! Async

let start ~port =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    (fun _address reader writer ->
       let ws = Websocket.create ~role:Server reader writer in
       let _wsreader, _wswriter = Websocket.pipes ws in
       let () =
         Websocket.monitor_pongs
           ~ping_every:(Time_ns.Span.of_int_sec 3)
           ~concerning_pong_response_delay:(Time_ns.Span.of_int_sec 10)
           ~on_concerning_pong_response_delay:(fun () ->
             Log.Global.info_s [%message "Lost connection!"])
           ws
       in
       (* let rec echo_loop () = *)
       (*   match Writer.is_closed writer with *)
       (*   | false -> *)
       (*     Log.Global.debug_s [%message "Send ping"]; *)
       (*     Websocket.send_ping ws "Hello\n"; *)
       (*     let%bind () = Pipe.write_if_open wswriter "Test" in *)
       (*     let%bind () = Clock.after (Time_float_unix.Span.of_ms 3000.) in *)
       (*     echo_loop () *)
       (*   | true -> *)
       (*     Log.Global.debug_s [%message "Bye"]; *)
       (*     Deferred.unit *)
       (* in *)
       (* echo_loop ()) *)
       never ())
  >>= Tcp.Server.close_finished
;;

open! Base
open! Core
open! Async

let start ~port =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    (fun _address reader writer ->
       let ws = Websocket.create ~role:Server reader writer in
       let _wsreader, wswriter = Websocket.pipes ws in
       let rec echo_loop () =
         match Writer.is_closed writer with
         | false ->
           Log.Global.debug_s [%message "Send ping"];
           Websocket.send_ping ws "Hello\n";
           let%bind () = Pipe.write_if_open wswriter "Test" in
           let%bind () = Clock.after (Time_float_unix.Span.of_ms 3000.) in
           echo_loop ()
         | true ->
           Log.Global.debug_s [%message "Bye"];
           Deferred.unit
       in
       echo_loop ())
  >>= Tcp.Server.close_finished
;;

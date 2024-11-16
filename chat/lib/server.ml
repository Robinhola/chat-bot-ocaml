open! Base
open! Core
open! Async

let with_can_write can_write ~f =
  let%bind () = Mvar.take can_write in
  let result = f () in
  let%map () = Mvar.put can_write () in
  result
;;

let enqueue_msg ~can_write ~msg_queues reader =
  Log.Global.debug_s [%message "Reading msg pipe..."];
  match%bind Pipe.read reader with
  | `Eof ->
    Log.Global.debug_s [%message "Bye!"];
    Deferred.unit
  | `Ok msg ->
    with_can_write can_write ~f:(fun () ->
      Log.Global.debug_s [%message "Read" (msg : string)];
      Log.Global.debug_s [%message "Update all msgs queues..."];
      Hashtbl.iteri msg_queues ~f:(fun ~key ~data:queue ->
        Log.Global.debug_s [%message "Adding msg into queue" (key : int)];
        Queue.enqueue queue msg))
;;

let write_server_message ~can_write ~msg_queues msg =
  with_can_write can_write ~f:(fun () ->
    Hashtbl.iteri msg_queues ~f:(fun ~key ~data:queue ->
      Log.Global.debug_s [%message "Adding msg into queue" (key : int)];
      Queue.enqueue queue msg))
;;

let user_left ~can_write ~msg_queues =
  let msg = "A user left the chat, bye!" in
  write_server_message ~can_write ~msg_queues msg
;;

let start ~port =
  let global_i = ref 0 in
  let index_to_msg_queue = Hashtbl.create (module Int) in
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    (fun _address reader writer ->
       let i = !global_i in
       global_i := !global_i + 1;
       Log.Global.info_s [%message "Hello!" (i : int)];
       let can_write = Mvar.create () in
       let () = Hashtbl.add_exn index_to_msg_queue ~key:i ~data:(Queue.create ()) in
       let ws = Websocket.create ~role:Server reader writer in
       let _wsreader, _wswriter = Websocket.pipes ws in
       let () =
         Websocket.monitor_pongs
           ~ping_every:(Time_ns.Span.of_int_sec 3)
           ~concerning_pong_response_delay:(Time_ns.Span.of_int_sec 10)
           ~on_concerning_pong_response_delay:(fun () ->
             Log.Global.info_s [%message "Lost connection!" (i : int)])
           ws
       in
       let wsreader, wswriter = Websocket.pipes ws in
       let receiver = Websocket.frame_received ws in
       (* We could unsubscribe from annoying clients *)
       let _bus =
         Bus.subscribe_exn receiver [%here] ~f:(function
           | Text ->
             enqueue_msg ~can_write ~msg_queues:index_to_msg_queue wsreader
             |> don't_wait_for
           | opcode -> Opcode.log opcode)
       in
       let%bind () = Mvar.put can_write () in
       let rec loop () =
         Log.Global.debug_s [%message "Echo loop" (i : int)];
         match Writer.is_closed writer with
         | false ->
           (* Use an Mvar here to make sure we don't erase messages while reading *)
           let%bind msgs =
             with_can_write can_write ~f:(fun () ->
               let queue = Hashtbl.find_exn index_to_msg_queue i in
               let msgs = Queue.to_list queue in
               let () = Queue.clear queue in
               msgs)
           in
           let%bind () =
             Deferred.List.iter ~how:`Sequential msgs ~f:(fun msg ->
               Log.Global.info_s [%message "Send msg" (i : int) (msg : string)];
               Pipe.write_if_open wswriter msg)
           in
           let%bind () = Clock.after (Time_float_unix.Span.of_ms 100.) in
           loop ()
         | true ->
           Log.Global.info_s [%message "Bye" (i : int)];
           Hashtbl.remove index_to_msg_queue i;
           user_left ~can_write ~msg_queues:index_to_msg_queue
       in
       loop ())
  >>= Tcp.Server.close_finished
;;

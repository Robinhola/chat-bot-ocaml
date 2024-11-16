open! Base
open! Core
open! Async

(* Use an Mvar to make sure we don't erase messages while reading *)
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

type 'a t =
  { writer : Writer.t
  ; can_write : (unit, 'a) Mvar.t
  ; id_to_msg_queue : (int, string Queue.t) Hashtbl.t
  ; ws : Websocket.t
  }

let rec loop ~id t =
  let { writer; can_write; id_to_msg_queue; ws } = t in
  Log.Global.debug_s [%message "Echo loop" (id : int)];
  match Writer.is_closed writer with
  | false ->
    let%bind msgs =
      with_can_write can_write ~f:(fun () ->
        let queue = Hashtbl.find_exn id_to_msg_queue id in
        let msgs = Queue.to_list queue in
        let () = Queue.clear queue in
        msgs)
    in
    let%bind () =
      Deferred.List.iter ~how:`Sequential msgs ~f:(fun msg ->
        let _, wswriter = Websocket.pipes ws in
        Log.Global.debug_s [%message "Send msg" (id : int) (msg : string)];
        Pipe.write_if_open wswriter msg)
    in
    let%bind () = Clock.after (Time_float_unix.Span.of_ms 100.) in
    loop ~id t
  | true ->
    Log.Global.info_s [%message "Bye" (id : int)];
    Hashtbl.remove id_to_msg_queue id;
    user_left ~can_write ~msg_queues:id_to_msg_queue
;;

let start ~port =
  let global_i = ref 0 in
  let id_to_msg_queue = Hashtbl.create (module Int) in
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    (fun _address reader writer ->
       let id = !global_i in
       global_i := !global_i + 1;
       Log.Global.info_s [%message "Hello!" (id : int)];
       let can_write = Mvar.create () in
       let () = Hashtbl.add_exn id_to_msg_queue ~key:id ~data:(Queue.create ()) in
       let ws = Websocket.create ~role:Server reader writer in
       let () =
         Websocket.monitor_pongs
           ~ping_every:(Time_ns.Span.of_int_sec 3)
           ~concerning_pong_response_delay:(Time_ns.Span.of_int_sec 10)
           ~on_concerning_pong_response_delay:(fun () ->
             Log.Global.info_s [%message "Lost connection!" (id : int)];
             don't_wait_for (Writer.close writer))
           ws
       in
       let receiver = Websocket.frame_received ws in
       (* We could unsubscribe from annoying clients *)
       let _bus =
         Bus.subscribe_exn receiver [%here] ~f:(function
           | Text ->
             let wsreader, _ = Websocket.pipes ws in
             enqueue_msg ~can_write ~msg_queues:id_to_msg_queue wsreader |> don't_wait_for
           | opcode -> Opcode.log opcode)
       in
       let%bind () = Mvar.put can_write () in
       loop ~id { writer; can_write; id_to_msg_queue; ws })
  >>= Tcp.Server.close_finished
;;

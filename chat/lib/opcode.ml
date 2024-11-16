open! Base
open! Core
open! Async

let log (opcode : Websocket.Opcode.t) =
  match opcode with
  | Continuation -> Log.Global.debug_s [%message "Continuation"]
  | Text -> Log.Global.debug_s [%message "Text"]
  | Binary -> Log.Global.debug_s [%message "Binary"]
  | Close -> Log.Global.debug_s [%message "Close"]
  | Ping -> Log.Global.debug_s [%message "Ping"]
  | Pong -> Log.Global.debug_s [%message "Pong"]
  | _ -> Log.Global.error_s [%message "Unused"]
;;

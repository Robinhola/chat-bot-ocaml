open! Base
open! Core
open! Async

type t = string

val random_name : unit -> t
val arg : unit -> t Command.Param.t

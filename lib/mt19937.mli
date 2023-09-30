open! Core

type t

val init : int -> t

val create_exn : Int32.t array -> t

val next : t -> int

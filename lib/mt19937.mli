open! Core

type t

val init : int -> t

val create_exn : int32 array -> t

val next' : t -> int32

val next : t -> int

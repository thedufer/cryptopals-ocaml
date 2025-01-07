open! Core

val decrypt : string -> blocksize:int -> key:'key -> cipher:(key:'key -> string -> string) -> string

val encrypt : string -> blocksize:int -> key:'key -> cipher:(key:'key -> string -> string) -> string

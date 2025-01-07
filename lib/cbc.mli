open! Core

val decrypt : string -> iv:string -> blocksize:int -> key:'key -> cipher:(key:'key -> string -> string) -> string

val encrypt : string -> iv:string -> blocksize:int -> key:'key -> cipher:(key:'key -> string -> string) -> string

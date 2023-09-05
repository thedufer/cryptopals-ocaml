open! Core

val decrypt : string -> blocksize:int -> key:'key -> cipher:(key:'key -> Cstruct.t -> Cstruct.t) -> string

val encrypt : string -> blocksize:int -> key:'key -> cipher:(key:'key -> Cstruct.t -> Cstruct.t) -> string

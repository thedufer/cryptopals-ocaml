open! Core

val decrypt : string -> iv:string -> blocksize:int -> key:'key -> cipher:(key:'key -> Cstruct.t -> Cstruct.t) -> string

val encrypt : string -> iv:string -> blocksize:int -> key:'key -> cipher:(key:'key -> Cstruct.t -> Cstruct.t) -> string

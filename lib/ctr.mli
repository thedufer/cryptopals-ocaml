open! Core

(** Implied blocksize of 16, with the counter a nonce concated to the block
    count (both little-endian int64s) *)
val encrypt : string -> nonce:int64 -> key:'key -> cipher:(key:'key -> string -> string) -> string

open! Core

val sha1_padding : len:int -> string
val sha1_seeded : h0:int32 -> h1:int32 -> h2:int32 -> h3:int32 -> h4:int32 -> prefix_len:int -> string -> string
val sha1 : string -> string

val md4_padding : len:int -> string
val md4_seeded : a:int32 -> b:int32 -> c:int32 -> d:int32 -> prefix_len:int -> string -> string
val md4 : string -> string

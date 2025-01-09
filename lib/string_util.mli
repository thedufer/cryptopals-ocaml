open! Core

val base64_to_raw : string -> string

val raw_to_base64 : string -> string

val hex_to_raw : string -> string

val raw_to_hex : string -> string

val xor_raw : string -> string -> string

(** Note: score scales with length; comparing scores of texts of different
    lengths won't make sense. *)
val english_score : string -> int

(** strings must be of the same length *)
val hamming_distance : string -> string -> int

val pkcs7_padding : string -> blocksize:int -> string

val remove_pkcs7_padding : string -> string

val random_bytes : int -> string

(** Last block won't be complete if the input isn't a multiple of [blocksize] in
    length *)
val to_blocks : string -> blocksize:int -> string list

val map_byte : string -> int -> f:(char -> char) -> string
val map_byte_i : string -> int -> f:(int -> int) -> string
val map_bytes_i : string -> (int * (int -> int)) list -> string

val sha1_padding : len:int -> string
val sha1_seeded : h0:int32 -> h1:int32 -> h2:int32 -> h3:int32 -> h4:int32 -> prefix_len:int -> string -> string
val sha1 : string -> string

val md4_padding : len:int -> string
val md4_seeded : a:int32 -> b:int32 -> c:int32 -> d:int32 -> prefix_len:int -> string -> string
val md4 : string -> string

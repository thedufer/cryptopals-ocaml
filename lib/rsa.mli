open! Core

type pub = [`Pub of Z.t * Z.t]
type priv = [`Priv of Z.t * Z.t]

val gen_rsa_keys : bytes:int -> pub * priv

val encrypt : pub -> Z.t -> Z.t

val decrypt : priv -> Z.t -> Z.t

(* helpers *)
val random_prime : bytes:int -> Z.t
val to_bits_trim : Z.t -> string

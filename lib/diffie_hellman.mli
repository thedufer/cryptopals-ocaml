open! Core

module Small : sig
  type pub
  type priv

  val gen_key : unit -> pub * priv
  val session : my_priv:priv -> peer_pub:pub -> Z.t
end

type pub
type priv

val gen_key : unit -> pub * priv
val session : my_priv:priv -> peer_pub:pub -> Z.t

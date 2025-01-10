open! Core

module type DHC = sig
  val p : Z.t
  val g : Z.t
end

module type S = sig
  include DHC

  type pub
  type priv

  val gen_key : unit -> pub * priv
  val session : my_priv:priv -> peer_pub:pub -> Z.t
end

module type Diffie_hellman = sig
  type pub
  type priv

  module Small : S with type pub := pub and type priv := priv

  include S with type pub := pub and type priv := priv

  val pub_of_z : Z.t -> pub

  module Make : DHC -> S with type pub := pub and type priv := priv
end

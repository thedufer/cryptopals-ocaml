open! Core

module type DHC = sig
  val p : Z.t
  val g : int
end

module Make (C : DHC) = struct
  open C
  type pub = Z.t
  type priv = Z.t

  let gen_key () =
    let priv = Z.random_int p in
    let pub = Z.powm (Z.of_int g) priv p in
    pub, priv

  let session ~my_priv ~peer_pub =
    Z.powm peer_pub my_priv p
end

module Small = Make (struct
    let p = Z.of_int 37
    let g = 5
  end)

include Make (struct
    let p = Z.of_string_base 16 "ffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb9ed529077096966d670c354e4abc9804f1746c08ca237327ffffffffffffffff"
    let g = 2
  end)

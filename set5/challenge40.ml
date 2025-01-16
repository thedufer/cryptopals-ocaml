open! Core
open! Import

let encrypt_with_new_key =
  let msg = Z.of_bits "this is a secret that's much longer" in
  fun () ->
    let pub, _priv = Rsa.gen_rsa_keys ~bytes:32 in
    let `Pub (_e, n) = pub in
    assert (Z.Compare.( < ) msg n);
    Rsa.encrypt pub msg, pub

let%expect_test "only 2?" =
  let c_0, `Pub (e_0, n_0) = encrypt_with_new_key () in
  let c_1, `Pub (e_1, n_1) = encrypt_with_new_key () in
  assert (Z.equal e_0 (Z.of_int 3));
  assert (Z.equal e_1 (Z.of_int 3));
  assert (Z.equal (Z.gcd n_0 n_1) Z.one);
  let ( + ) = Z.( + ) in
  let ( * ) = Z.( * ) in
  let m_s_0 = n_1 in
  let m_s_1 = n_0 in
  let result =
    Z.root
      (Z.( mod )
         (c_0 * m_s_0 * Z.invert m_s_0 n_0 +
          c_1 * m_s_1 * Z.invert m_s_1 n_1)
         (n_0 * n_1))
      3
  in
  print_s [%sexp (Rsa.to_bits_trim result : string)];
  [%expect {| "this is a secret that's much longer" |}]

let%expect_test "final" =
  let c_0, `Pub (e_0, n_0) = encrypt_with_new_key () in
  let c_1, `Pub (e_1, n_1) = encrypt_with_new_key () in
  let c_2, `Pub (e_2, n_2) = encrypt_with_new_key () in
  assert (Z.equal e_0 (Z.of_int 3));
  assert (Z.equal e_1 (Z.of_int 3));
  assert (Z.equal e_2 (Z.of_int 3));
  assert (Z.equal (Z.gcd n_0 n_1) Z.one);
  assert (Z.equal (Z.gcd n_1 n_2) Z.one);
  assert (Z.equal (Z.gcd n_2 n_0) Z.one);
  let ( + ) = Z.( + ) in
  let ( * ) = Z.( * ) in
  let m_s_0 = n_1 * n_2 in
  let m_s_1 = n_0 * n_2 in
  let m_s_2 = n_0 * n_1 in
  let result =
    Z.root
      (Z.( mod )
         (c_0 * m_s_0 * Z.invert m_s_0 n_0 +
          c_1 * m_s_1 * Z.invert m_s_1 n_1 +
          c_2 * m_s_2 * Z.invert m_s_2 n_2)
         (n_0 * n_1 * n_2))
      3
  in
  print_s [%sexp (Rsa.to_bits_trim result : string)];
  [%expect {| "this is a secret that's much longer" |}]

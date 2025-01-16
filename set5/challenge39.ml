open Core
open! Import

let%expect_test "primegen" =
  let test () =
    Rsa.random_prime ~bytes:1 |> Z.to_string |> print_endline
  in
  test ();
  test ();
  test ();
  [%expect {|
    197
    251
    101 |}]

let egcd a b =
  let old_r, r = a, b in
  let old_s, s = Z.one, Z.zero in
  let old_t, t = Z.zero, Z.one in
  let rec aux old_r r old_s s old_t t =
    let q = Z.div old_r r in
    let old_r, r = r, Z.sub old_r (Z.mul q r) in
    let old_s, s = s, Z.sub old_s (Z.mul q s) in
    let old_t, t = t, Z.sub old_t (Z.mul q t) in
    if Z.equal r Z.zero then
      (* Bezout coefficients, gcd, quotients by gcd *)
      old_s, old_t, old_r, t, s
    else
      aux old_r r old_s s old_t t
  in
  aux old_r r old_s s old_t t

let invmod a m =
  let x, _y, gcd, _, _ = egcd a m in
  assert (Z.equal gcd Z.one);
  x

let%expect_test "rsa example" =
  let p, q = Z.of_int 17, Z.of_int 59 in
  let n = Z.mul p q in
  let et = Z.mul (Z.pred p) (Z.pred q) in
  let e = Z.of_int 3 in
  let d = invmod e et in
  let _pub = e, n in
  let _priv = d, n in
  let orig_plaintext = Z.of_int 42 in
  let cipher = Z.powm orig_plaintext e n in
  let plaintext = Z.powm cipher d n in
  assert (Z.equal orig_plaintext plaintext);
  [%expect {||}]

let%expect_test "big rsa" =
  let pub, priv = Rsa.gen_rsa_keys ~bytes:32 in
  let msg = "this is a bit longer" in
  let cipher = Rsa.encrypt pub (Z.of_bits msg) in
  let plain = Rsa.decrypt priv cipher |> Rsa.to_bits_trim in
  assert (String.equal plain msg);
  [%expect {||}]

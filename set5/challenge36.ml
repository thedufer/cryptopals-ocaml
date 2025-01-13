open Core
open! Import

let g = Diffie_hellman.g
let n = Diffie_hellman.p
let k = Z.of_int 3
let email = "test@example.com"
let password = "a secret"

let s_init () =
  let salt = String_util.random_bytes 16 in
  let xh = Digestif.SHA256.digest_string (salt ^ password) |> Digestif.SHA256.to_raw_string in
  let x = Z.of_bits xh in
  let v = Z.powm g x n in
  (salt, v)

let c_first_message () =
  let a = Z.random_int n in
  let big_a = Z.powm g a n in
  (big_a, a), (email, big_a)

let s_second_message (salt, v) (email, big_a) =
  let b = Z.random_int n in
  (* is there another mod n at the end of this? *)
  let big_b = Z.(mod) (Z.add (Z.powm g b n) (Z.mul k v)) n in
  (big_b, b, salt, v, email, big_a), (salt, big_b)

let u big_a big_b =
  Digestif.SHA256.digest_string (Z.to_bits big_a ^ Z.to_bits big_b)
  |> Digestif.SHA256.to_raw_string
  |> Z.of_bits

let c_third_message (big_a, a) (salt, big_b) =
  let xh = Digestif.SHA256.digest_string (salt ^ password) |> Digestif.SHA256.to_raw_string in
  let x = Z.of_bits xh in
  let u = u big_a big_b in
  let big_s = Z.powm (Z.sub big_b (Z.mul k (Z.powm g x n))) (Z.add a (Z.mul u x)) n in
  let big_k = Z.to_bits big_s |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_raw_string in
  (* this should be hmac-sha256, which isn't implemented *)
  Digestif.SHA256.hmac_string ~key:salt big_k |> Digestif.SHA256.to_raw_string

let s_validate (big_b, b, salt, v, _email, big_a) signature =
  let u = u big_a big_b in
  let big_s = Z.powm (Z.mul big_a (Z.powm v u n)) b n in
  let big_k = Z.to_bits big_s |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_raw_string in
  let expected_signature = Digestif.SHA256.hmac_string ~key:salt big_k |> Digestif.SHA256.to_raw_string in
  assert (String.equal signature expected_signature);
  ()

let%expect_test "final" =
  let s_context = s_init () in
  let c_context, first_message = c_first_message () in
  let s_context, second_message = s_second_message s_context first_message in
  let third_message = c_third_message c_context second_message in
  let () = s_validate s_context third_message in
  [%expect {||}]

open! Core
open! Import

let g = Challenge36.g
let k = Challenge36.k
let n = Challenge36.n

let dict = lazy (In_channel.read_lines "/usr/share/dict/words")

let s_init ~email ~password =
  let salt = String_util.random_bytes 16 in
  let x = Hash.sha256 (salt ^ password) |> Z.of_bits in
  let v = Z.powm g x n in
  (email, salt, v)

let c_first_message ~email ~password =
  let a = Z.random_int n in
  let big_a = Z.powm g a n in
  (a, password), (email, big_a)

let s_second_message (s_email, salt, v) (c_email, big_a) =
  assert (String.equal s_email c_email);
  let u = String_util.random_bytes 16 |> Z.of_bits in
  let b = Z.random_int n in
  let big_b = Z.powm g b n in
  (s_email, v, u, big_a, salt, b), (salt, big_b, u)

let c_third_message (a, password) (salt, big_b, u) =
  let x = Hash.sha256 (salt ^ password) |> Z.of_bits in
  let big_s = Z.powm big_b (Z.add a (Z.mul u x)) n in
  let big_k = Z.to_bits big_s |> Hash.sha256 in
  Hash.hmac_sha256 ~key:big_k salt

let s_validate (email, v, u, big_a, salt, b) signature =
  let big_s = Z.powm (Z.mul big_a (Z.powm v u n)) b n in
  let big_k = Z.to_bits big_s |> Hash.sha256 in
  let expected_signature = Hash.hmac_sha256 ~key:big_k salt in
  assert (String.equal expected_signature signature);
  email

let%expect_test "protocol" =
  let email = "test@example.com" in
  let password = "a secret" in
  let s_context = s_init ~email ~password in
  let c_context, first_message = c_first_message ~email ~password in
  let s_context, second_message = s_second_message s_context first_message in
  let third_message = c_third_message c_context second_message in
  let _email = s_validate s_context third_message in
  [%expect {||}]

let s_att_init () =
  let salt = String_util.random_bytes 16 in
  salt

let s_att_second_message salt (email, big_a) =
  let u = String_util.random_bytes 16 |> Z.of_bits in
  let b = Z.random_int n in
  let big_b = Z.powm g b n in
  (email, u, big_a, salt, b), (salt, big_b, u)

let s_att_crack (email, u, big_a, salt, b) signature =
  let try_password password =
    let x = Hash.sha256 (salt ^ password) |> Z.of_bits in
    let v = Z.powm g x n in
    let big_s = Z.powm (Z.mul big_a (Z.powm v u n)) b n in
    let big_k = Z.to_bits big_s |> Hash.sha256 in
    let expected_signature = Hash.hmac_sha256 ~key:big_k salt in
    String.equal expected_signature signature
  in
  let password =
    List.find_exn (force dict) ~f:try_password
  in
  (email, password)

let%expect_test "offline attack" =
  let email = "test@example.com" in
  let password = List.nth_exn (force dict) (Random.int (List.length (force dict))) in
  let s_context = s_att_init () in
  let c_context, first_message = c_first_message ~email ~password in
  let s_context, second_message = s_att_second_message s_context first_message in
  let third_message = c_third_message c_context second_message in
  let email, f_password = s_att_crack s_context third_message in
  print_s [%sexp (email, f_password, password : string * string * string)];
  [%expect {| (test@example.com angleberry angleberry) |}]

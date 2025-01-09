open Core
open! Import

let blocksize = 16

(* Each of these returns a context to keep on the current side, and a message to
   send to the other side *)

let a_get_first_message real_message =
  let big_a, a = Diffie_hellman.gen_key () in
  (a, real_message), (Diffie_hellman.p, Diffie_hellman.g, big_a)

let b_get_second_message (p, g, big_a) =
  let module DHB = Diffie_hellman.Make (struct let p = p let g = g end) in
  let big_b, b = DHB.gen_key () in
  (p, g, b, big_a), big_b

let a_get_third_message (a, real_message) big_b =
  let s = Diffie_hellman.session ~my_priv:a ~peer_pub:big_b |> Z.to_bits in
  let key = Hash.sha1 s |> String.subo ~len:16 |> Mirage_crypto.AES.ECB.of_secret in
  let iv = String_util.random_bytes 16 in
  let encrypted = Cbc.encrypt real_message ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.encrypt ~key in
  key, (encrypted, iv)

let b_get_fourth_message (p, g, b, big_a) (encrypted, iv) =
  let module DHB = Diffie_hellman.Make (struct let p = p let g = g end) in
  let s = DHB.session ~my_priv:b ~peer_pub:big_a |> Z.to_bits in
  let key = Hash.sha1 s |> String.subo ~len:16 |> Mirage_crypto.AES.ECB.of_secret in
  let decrypted = Cbc.decrypt encrypted ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.decrypt ~key in
  let iv = String_util.random_bytes 16 in
  let re_encrypted = Cbc.encrypt decrypted ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.encrypt ~key in
  re_encrypted, iv

let a_decrypt_fourth_message key (encrypted, iv) =
  Cbc.decrypt encrypted ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.decrypt ~key

let%expect_test "protocol" =
  let real_message = "this is A" in
  (* as a, set up the first message *)
  let a_context, first_message = a_get_first_message real_message in
  (* as b, set up the second message, using the first *)
  let b_context, second_message = b_get_second_message first_message in
  (* as a, send the actual message encrypted with the DH session key *)
  let a_context, third_message = a_get_third_message a_context second_message in
  (* as b, decrypt the message and send it back encrypted with a different iv *)
  let fourth_message = b_get_fourth_message b_context third_message in
  (* as a, decrypt the last message and ensure it matches what we originally sent *)
  let round_tripped_message = a_decrypt_fourth_message a_context fourth_message in
  assert (String.equal real_message round_tripped_message);
  [%expect {||}]

let m_corrupt_first_message (p, g, _big_a) =
  (p, g), (p, g, Diffie_hellman.pub_of_z p)

let m_corrupt_second_message (p, g) _big_b =
  (p, g), Diffie_hellman.pub_of_z p

let m_decrypt_third_message (p, g) (encrypted, iv) =
  let module DHM = Diffie_hellman.Make (struct let p = p let g = g end) in
  let s = Z.to_bits Z.zero in
  let key = Hash.sha1 s |> String.subo ~len:16 |> Mirage_crypto.AES.ECB.of_secret in
  Cbc.decrypt encrypted ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.decrypt ~key:key

let m_decrypt_fourth_message = m_decrypt_third_message

let%expect_test "final" =
  let real_message = "this is A" in
  (* as a, set up the first message *)
  let a_context, first_message = a_get_first_message real_message in
  (* as m, corrupt the first message *)
  let m_context, first_message = m_corrupt_first_message first_message in
  (* as b, set up the second message, using the first *)
  let b_context, second_message = b_get_second_message first_message in
  (* as m, corrupt the second message *)
  let m_context, second_message = m_corrupt_second_message m_context second_message in
  (* as a, send the actual message encrypted with the DH session key *)
  let a_context, third_message = a_get_third_message a_context second_message in
  (* as b, decrypt the message and send it back encrypted with a different iv *)
  let fourth_message = b_get_fourth_message b_context third_message in
  (* as a, decrypt the last message and ensure it matches what we originally sent *)
  let round_tripped_message = a_decrypt_fourth_message a_context fourth_message in
  assert (String.equal real_message round_tripped_message);
  (* as m, decrypt the third and fourth messages, and make sure they match the real message *)
  let decrypted_third_message = m_decrypt_third_message m_context third_message in
  assert (String.equal real_message decrypted_third_message);
  let decrypted_fourth_message = m_decrypt_fourth_message m_context fourth_message in
  assert (String.equal real_message decrypted_fourth_message);
  [%expect {||}]

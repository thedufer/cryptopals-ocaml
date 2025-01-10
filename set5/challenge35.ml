open Core
open! Import

let blocksize = 16

(* set g=1
   this causes B to always be 1, which means that party a derives a session key of 1
   however, party b derives a different session key (because a and b have different values of g)
   we can decrypt encrypted messages sent by a, but b can't, so they'll notice
   pretty quickly that something is up *)

let m_corrupt_first_message (p, _g, big_a) =
  let g = Z.one in
  (p, g), (p, g, big_a)

let m_corrupt_second_message (p, g) big_b =
  (p, g), big_b

let m_decrypt_third_message (p, g) (encrypted, iv) =
  let module DHM = Diffie_hellman.Make (struct let p = p let g = g end) in
  let s = Z.to_bits Z.one in
  let key = Hash.sha1 s |> String.subo ~len:16 |> Mirage_crypto.AES.ECB.of_secret in
  Cbc.decrypt encrypted ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.decrypt ~key:key

let%expect_test "final" =
  let real_message = "this is A" in
  (* as a, set up the first message *)
  let a_context, first_message = Challenge34.a_get_first_message real_message in
  (* as m, corrupt the first message *)
  let m_context, first_message = m_corrupt_first_message first_message in
  (* as b, set up the second message, using the first *)
  let b_context, second_message = Challenge34.b_get_second_message first_message in
  (* as m, corrupt the second message *)
  let m_context, second_message = m_corrupt_second_message m_context second_message in
  (* as a, send the actual message encrypted with the DH session key *)
  let _a_context, third_message = Challenge34.a_get_third_message a_context second_message in
  (* as b, try to decrypt the message and send it back encrypted with a different iv

     this doesn't work because g was corrupted and a/b have different session keys *)
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
      Challenge34.b_get_fourth_message b_context third_message);
  [%expect {| (Invalid_argument "index out of bounds") |}];
  (* as m, decrypt the third message, and make sure it matches the real message *)
  let decrypted_third_message = m_decrypt_third_message m_context third_message in
  assert (String.equal real_message decrypted_third_message);
  [%expect {||}]

(* set g=p
   this forces B to be 0, so party a generates a session key of 0
   again, we can decrypt messages that a sends to be, but b can't, so they'll
   notice *)

let m_corrupt_first_message (p, _g, big_a) =
  let g = p in
  (p, g), (p, g, big_a)

let m_corrupt_second_message (p, g) big_b =
  (p, g), big_b

let m_decrypt_third_message (p, g) (encrypted, iv) =
  let module DHM = Diffie_hellman.Make (struct let p = p let g = g end) in
  let s = Z.to_bits Z.zero in
  let key = Hash.sha1 s |> String.subo ~len:16 |> Mirage_crypto.AES.ECB.of_secret in
  Cbc.decrypt encrypted ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.decrypt ~key:key

let%expect_test "final" =
  let real_message = "this is A" in
  (* as a, set up the first message *)
  let a_context, first_message = Challenge34.a_get_first_message real_message in
  (* as m, corrupt the first message *)
  let m_context, first_message = m_corrupt_first_message first_message in
  (* as b, set up the second message, using the first *)
  let b_context, second_message = Challenge34.b_get_second_message first_message in
  (* as m, corrupt the second message *)
  let m_context, second_message = m_corrupt_second_message m_context second_message in
  (* as a, send the actual message encrypted with the DH session key *)
  let _a_context, third_message = Challenge34.a_get_third_message a_context second_message in
  (* as b, try to decrypt the message and send it back encrypted with a different iv

     this doesn't work because g was corrupted and a/b have different session keys *)
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
      Challenge34.b_get_fourth_message b_context third_message);
  (* as m, decrypt the third and fourth messages, and make sure they match the real message *)
  let decrypted_third_message = m_decrypt_third_message m_context third_message in
  assert (String.equal real_message decrypted_third_message);
  [%expect {| (Invalid_argument "index out of bounds") |}]

(* set g=p-1
   this forces B to be either p-1 or 1 depending on the parity of its priv key,
   so party a generates a session key of p-1 or 1
   again, we can decrypt messages that a sends to be, but b can't, so they'll
   notice
   it does take us two tries, though, so this is kind of an annoying choice of g
*)

let m_corrupt_first_message (p, _g, big_a) =
  let g = Z.sub p Z.one in
  (p, g), (p, g, big_a)

let m_corrupt_second_message (p, g) big_b =
  (p, g), big_b

let m_decrypt_third_message (p, g) (encrypted, iv) =
  let module DHM = Diffie_hellman.Make (struct let p = p let g = g end) in
  let decrypt s =
    let s = Z.to_bits s in
    let key = Hash.sha1 s |> String.subo ~len:16 |> Mirage_crypto.AES.ECB.of_secret in
    Cbc.decrypt encrypted ~iv ~blocksize ~cipher:Mirage_crypto.AES.ECB.decrypt ~key:key
  in
  try decrypt (Z.sub p Z.one) with
  | _ -> decrypt Z.one

let%expect_test "final" =
  let real_message = "this is A" in
  (* as a, set up the first message *)
  let a_context, first_message = Challenge34.a_get_first_message real_message in
  (* as m, corrupt the first message *)
  let m_context, first_message = m_corrupt_first_message first_message in
  (* as b, set up the second message, using the first *)
  let b_context, second_message = Challenge34.b_get_second_message first_message in
  (* as m, corrupt the second message *)
  let m_context, second_message = m_corrupt_second_message m_context second_message in
  (* as a, send the actual message encrypted with the DH session key *)
  let _a_context, third_message = Challenge34.a_get_third_message a_context second_message in
  (* as b, try to decrypt the message and send it back encrypted with a different iv

     this doesn't work because g was corrupted and a/b have different session keys *)
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
      Challenge34.b_get_fourth_message b_context third_message);
  (* as m, decrypt the third and fourth messages, and make sure they match the real message *)
  let decrypted_third_message = m_decrypt_third_message m_context third_message in
  assert (String.equal real_message decrypted_third_message);
  [%expect {| (Invalid_argument "index out of bounds") |}]

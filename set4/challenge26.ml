open! Core
open! Import

let oracle, check_is_admin =
  let open Mirage_crypto.AES.ECB in
  let key = String_util.random_bytes 16 |> of_secret in
  let prefix = "comment1=cooking%20MCs;userdata=" in
  let suffix = ";comment2=%20like%20a%20pound%20of%20bacon" in
  let clean = String.tr_multi ~target:";=" ~replacement:"--" |> unstage in
  let oracle user_input =
    let plaintext = prefix ^ clean user_input ^ suffix in
    Ctr.encrypt plaintext ~nonce:0L ~key ~cipher:encrypt
  in
  let check_is_admin ciphertext =
    let plaintext = Ctr.encrypt ciphertext ~nonce:0L ~key ~cipher:encrypt in
    print_endline plaintext;
    String.split plaintext ~on:';'
    |> List.map ~f:(String.lsplit2_exn ~on:'=')
    |> List.exists ~f:(function
        | "admin", "true" -> true
        | _ -> false)
  in
  oracle, check_is_admin

let solve oracle =
  (* We input almost what we want, but with the illegal characters swapped by a
     bit each. Then we just flip the corresponding bits. *)
  let input = ":admin<true" in
  let ciphertext = oracle input in
  (* now flip the low bit in the 32nd and 38th characters *)
  let ciphertext_b = Bytes.of_string ciphertext in
  let flip_low_bit c = Char.of_int_exn (Char.to_int c lxor 1) in
  Bytes.set ciphertext_b 32 (Bytes.get ciphertext_b 32 |> flip_low_bit);
  Bytes.set ciphertext_b 38 (Bytes.get ciphertext_b 38 |> flip_low_bit);
  Bytes.to_string ciphertext_b

let%expect_test "final" =
  let ciphertext = solve oracle in
  check_is_admin ciphertext |> Bool.to_string |> print_endline;
  [%expect {|
    comment1=cooking%20MCs;userdata=;admin=true;comment2=%20like%20a%20pound%20of%20bacon
    true |}]

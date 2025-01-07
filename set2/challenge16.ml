open! Core
open! Import

let oracle, check_is_admin =
  let open Mirage_crypto.AES.ECB in
  let key = String_util.random_bytes 16 |> of_secret in
  let iv = String.init 16 ~f:(const '\x00') in
  let blocksize = 16 in
  let prefix = "comment1=cooking%20MCs;userdata=" in
  let suffix = ";comment2=%20like%20a%20pound%20of%20bacon" in
  let clean = String.tr_multi ~target:";=" ~replacement:"--" |> unstage in
  let oracle user_input =
    let plaintext = prefix ^ clean user_input ^ suffix in
    Cbc.encrypt plaintext ~iv ~key ~cipher:encrypt ~blocksize
  in
  let check_is_admin ciphertext =
    let plaintext = Cbc.decrypt ciphertext ~iv ~key ~cipher:decrypt ~blocksize in
    print_endline plaintext;
    String.split plaintext ~on:';'
    |> List.map ~f:(String.lsplit2_exn ~on:'=')
    |> List.exists ~f:(function
        | "admin", "true" -> true
        | _ -> false)
  in
  oracle, check_is_admin

let solve oracle =
  (* the prefix is exactly two blocks, so that's nice
     let's put in two blocks. the first doesn't matter.
     then the second block should end with something close to what we want, but
     without the disallowed characters
     now bitflip them into the correct characters
  *)
  let input = String.init 21 ~f:(const 'B') ^ ":admin<true" in
  let ciphertext = oracle input in
  (* now flip the low bit in the 53rd and 59th characters of the plaintext, but
     flipping the low bit in the 37th and 43rd characters of the ciphertext *)
  let ciphertext_b = Bytes.of_string ciphertext in
  let flip_low_bit c = Char.of_int_exn (Char.to_int c lxor 1) in
  Bytes.set ciphertext_b 37 (Bytes.get ciphertext_b 37 |> flip_low_bit);
  Bytes.set ciphertext_b 43 (Bytes.get ciphertext_b 43 |> flip_low_bit);
  Bytes.to_string ciphertext_b

let%expect_test "final" =
  let ciphertext = solve oracle in
  check_is_admin ciphertext |> Bool.to_string |> print_endline;
  [%expect {|
    comment1=cooking%20MCs;userdata=*æ£∞ìÙõç~)Ê`(K‰BBBBB;admin=true;comment2=%20like%20a%20pound%20of%20bacon
    true |}]

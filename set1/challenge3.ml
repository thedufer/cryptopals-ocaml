open! Core
open! Import

let solve_single_char_xor input =
  let key = Xor.find_single_char_cipher_key input in
  Xor.single_char_cipher input key

let%expect_test "final" =
  let raw = String_util.hex_to_raw "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" in
  solve_single_char_xor raw |> print_endline;
  [%expect {| Cooking MC's like a pound of bacon |}]

open! Core
open! Import

let%expect_test "hex conversions" =
  String_util.hex_to_raw "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  |> String_util.raw_to_hex
  |> print_endline;
  [%expect {| 49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d |}]

let%expect_test "final" =
  String_util.xor_raw
    (String_util.hex_to_raw "1c0111001f010100061a024b53535009181c")
    (String_util.hex_to_raw "686974207468652062756c6c277320657965")
  |> String_util.raw_to_hex
  |> print_endline;
  [%expect {| 746865206b696420646f6e277420706c6179 |}]

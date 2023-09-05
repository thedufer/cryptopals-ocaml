open! Core
open! Import

let%expect_test "final" =
  String_util.hex_to_raw "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  |> String_util.raw_to_base64
  |> print_endline;
  [%expect {| SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t |}]

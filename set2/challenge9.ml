open! Core
open! Import

let%expect_test "final" =
  let result = String_util.pkcs7_padding "YELLOW SUBMARINE" ~blocksize:20 in
  print_s [%sexp (result : string)];
  [%expect {| "YELLOW SUBMARINE\004\004\004\004" |}]

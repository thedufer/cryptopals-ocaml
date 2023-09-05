open! Core
open! Import

let%expect_test "final" =
  String_util.remove_pkcs7_padding "ICE ICE BABY\x04\x04\x04\x04"
  |> print_endline;
  Expect_test_helpers_core.require_does_raise ~hide_positions:true [%here] (fun () ->
      String_util.remove_pkcs7_padding "ICE ICE BABY\x05\x05\x05\x05");
  Expect_test_helpers_core.require_does_raise ~hide_positions:true [%here] (fun () ->
      String_util.remove_pkcs7_padding "ICE ICE BABY\x01\x02\x03\x04");
  [%expect {|
    ICE ICE BABY
    "Assert_failure lib/string_util.ml:LINE:COL"
    "Assert_failure lib/string_util.ml:LINE:COL" |}]

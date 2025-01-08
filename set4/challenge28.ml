open Core
open Import

let sha1_auth ~key msg =
  String_util.sha1 (key ^ msg)

let%expect_test "sha1" =
  String_util.sha1 "The quick brown fox jumps over the lazy dog" |> String_util.raw_to_hex |> print_endline;
  [%expect {|2fd4e1c67a2d28fced849ee1bb76e7391b93eb12|}]

let%expect_test "auth" =
  let test msg =
    sha1_auth ~key:"YELLOW_SUBMARINE" msg
    |> String_util.raw_to_hex
    |> print_endline
  in
  test "";
  test "hi";
  test "i dunno";
  [%expect {|
    1faddaf1fabc4b096e5bc3891ada2584c1727189
    e3a3df1c2ceeb09d220d868a17b2400029ea7784
    3c49550f50c1d5da05828ef7793b7b7b527fadf9 |}]

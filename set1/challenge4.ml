open! Core
open! Import

let solve () =
  let input = In_channel.read_lines "challenge4.txt" in
  List.map input ~f:(fun line ->
      let line = String_util.hex_to_raw line in
      let key = Xor.find_single_char_cipher_key line in
      let result = Xor.single_char_cipher line key in
      let score = String_util.english_score result in
      (score, result))
  |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:fst)
  |> Option.value_exn
  |> snd

let%expect_test "final" =
  solve () |> print_endline;
  [%expect {| Now that the party is jumping |}]

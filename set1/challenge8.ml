open! Core
open! Import

let get_raw_data () =
  let input = In_channel.read_lines "challenge8.txt" in
  List.map input ~f:String_util.hex_to_raw

let solve () =
  get_raw_data ()
  |> List.map ~f:(fun data ->
      let unique_blocks =
        String_util.to_blocks data ~blocksize:16
        |> String.Set.of_list
      in
      data, Set.length unique_blocks)
  |> List.min_elt ~compare:(Comparable.lift ~f:snd Int.compare)
  |> Option.value_exn
  |> fst |> String_util.raw_to_hex

let%expect_test "final" =
  solve () |> print_endline;
  [%expect {| d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a |}]

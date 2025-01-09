open Core
open Import

let orig_val = "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"

let md4_auth ~key msg =
  Hash.md4 (key ^ msg)

let orig_hash, verify, key =
  let key = String_util.random_bytes 16 in
  let hash = md4_auth ~key orig_val in
  let verify val_ ~hash =
    let expected_hash = md4_auth ~key val_ in
    String.equal expected_hash hash
  in
  hash, verify, key

let solve ~orig_hash ~orig_val verify =
  let new_message = ";admin=true" in
  List.init 128 ~f:Fn.id
  |> List.find_map ~f:(fun key_len ->
      let glue = Hash.md4_padding ~len:(key_len + String.length orig_val) in
      let new_hash =
        let orig_hash_bs = Bigstring.of_string orig_hash in
        let a = Bigstring.get_int32_t_le orig_hash_bs ~pos:0 in
        let b = Bigstring.get_int32_t_le orig_hash_bs ~pos:4 in
        let c = Bigstring.get_int32_t_le orig_hash_bs ~pos:8 in
        let d = Bigstring.get_int32_t_le orig_hash_bs ~pos:12 in
        Hash.md4_seeded ~a ~b ~c ~d ~prefix_len:(key_len + String.length orig_val + String.length glue) new_message
      in
      let new_val = orig_val ^ glue ^ new_message in
      if verify new_val ~hash:new_hash then Some (new_val, new_hash) else None)
  |> Option.value_exn

let%expect_test "md4" =
  Hash.md4 "" |> String_util.raw_to_hex |> print_endline;
  Hash.md4 "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
  |> String_util.raw_to_hex |> print_endline;
  [%expect {|
    31d6cfe0d16ae931b73c59d7e0c089c0
    e33b4ddc9c38f2199c3e7b164fcc0536 |}]

let%expect_test "final" =
  let new_val, new_hash = solve ~orig_hash ~orig_val verify in
  verify new_val ~hash:new_hash |> Bool.to_string |> print_endline;
  [%expect {| true |}]

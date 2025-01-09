open Core
open Import

let orig_val = "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"

let sha1_auth ~key msg =
  Hash.sha1 (key ^ msg)

let orig_hash, verify, key =
  let key = String_util.random_bytes 16 in
  let hash = sha1_auth ~key orig_val in
  let verify val_ ~hash =
    let expected_hash = sha1_auth ~key val_ in
    String.equal expected_hash hash
  in
  hash, verify, key

let solve ~orig_hash ~orig_val verify =
  let new_message = ";admin=true" in
  List.init 128 ~f:Fn.id
  |> List.find_map ~f:(fun key_len ->
      let glue = Hash.sha1_padding ~len:(key_len + String.length orig_val) in
      let new_hash =
        let orig_hash_bs = Bigstring.of_string orig_hash in
        let h0 = Bigstring.get_int32_t_be orig_hash_bs ~pos:0 in
        let h1 = Bigstring.get_int32_t_be orig_hash_bs ~pos:4 in
        let h2 = Bigstring.get_int32_t_be orig_hash_bs ~pos:8 in
        let h3 = Bigstring.get_int32_t_be orig_hash_bs ~pos:12 in
        let h4 = Bigstring.get_int32_t_be orig_hash_bs ~pos:16 in
        Hash.sha1_seeded ~h0 ~h1 ~h2 ~h3 ~h4 ~prefix_len:(key_len + String.length orig_val + String.length glue) new_message
      in
      let new_val = orig_val ^ glue ^ new_message in
      if verify new_val ~hash:new_hash then Some (new_val, new_hash) else None)
  |> Option.value_exn

let%expect_test "final" =
  let new_val, new_hash = solve ~orig_hash ~orig_val verify in
  verify new_val ~hash:new_hash |> Bool.to_string |> print_endline;
  [%expect {| true |}]

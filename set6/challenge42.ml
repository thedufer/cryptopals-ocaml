open Core
open! Import

let encode msg =
  let algoid =
    let md5_oid =
      String_util.hex_to_raw "06082a864886f70d0205"
    in
    let null = String_util.hex_to_raw "0500" in
    String_util.hex_to_raw "30" ^
    (Char.of_int_exn (String.length md5_oid + String.length null) |> String.of_char) ^
    md5_oid ^
    null
  in
  let hash_encoding =
    let hash = Digestif.MD5.digest_string msg |> Digestif.MD5.to_raw_string in
    String_util.hex_to_raw "30" ^ (Char.of_int_exn (String.length hash) |> String.of_char) ^ hash
  in
  String_util.hex_to_raw "30" ^
  (Char.of_int_exn (String.length algoid + String.length hash_encoding) |> String.of_char) ^
  algoid ^
  hash_encoding

let verifier, pub, priv =
  let pub, priv = Rsa.gen_rsa_keys ~bytes:64 in
  let verifier signature msg =
    let msg = encode msg in
    let result = String.of_char '\000' ^ (Rsa.encrypt pub signature |> Rsa.to_bits_trim |> String.rev) in
    match String.substr_index result ~pattern:msg with
    | None -> false
    | Some i ->
      Char.equal (String.get result 0) '\000' &&
      Char.equal (String.get result 1) '\001' &&
      Char.equal (String.get result (i - 1)) '\000' &&
      String.for_all (String.slice result 2 (i - 1)) ~f:(Char.equal '\255')
  in
  verifier, pub, priv

let%expect_test "verifier" =
  let msg = "hi mom" in
  let encoded_with_padding = String_util.hex_to_raw "0001ffffff00" ^ encode msg in
  let signature = Rsa.decrypt priv (Z.of_bits (encoded_with_padding |> String.rev)) in
  print_s [%sexp (verifier signature msg : bool)];
  [%expect {| true |}]

let forge msg verifier =
  let encoded = String_util.hex_to_raw "0001ff00" ^ encode msg in
  let with_trailing_zeroes = encoded ^ String.make 89 '\000' in
  let root = Z.root (Z.of_bits (String.rev with_trailing_zeroes)) 3 in
  if verifier root msg then
    root
  else
    Z.succ root

let%expect_test "final" =
  let msg = "hi mom" in
  let signature = forge msg verifier in
  print_s [%sexp (verifier signature msg : bool)];
  [%expect {| true |}]

open! Core

let encrypt input ~nonce ~key ~cipher =
  let cipher = cipher ~key in
  let blocks = String_util.to_blocks input ~blocksize:16 in
  let key_plaintext = Bytes.create 16 in
  Stdlib.Bytes.set_int64_le key_plaintext 0 nonce;
  List.mapi blocks ~f:(fun i block ->
      Stdlib.Bytes.set_int64_le key_plaintext 8 (Int.to_int64 i);
      let key = cipher (Bytes.to_string key_plaintext) |> String.sub ~pos:0 ~len:(String.length block) in
      String_util.xor_raw block key)
  |> String.concat

open! Core

let decrypt input ~iv ~blocksize ~key ~cipher =
  let cipher = cipher ~key in
  let blocks = String_util.to_blocks input ~blocksize in
  List.folding_map blocks ~init:iv ~f:(fun prev_ciphertext next_ciphertext ->
      let next_plaintext =
        next_ciphertext
        |> Cstruct.of_string
        |> cipher
        |> Cstruct.to_string
        |> String_util.xor_raw prev_ciphertext
      in
      next_ciphertext, next_plaintext)
  |> String.concat
  |> String_util.remove_pkcs7_padding

let encrypt input ~iv ~blocksize ~key ~cipher =
  let cipher = cipher ~key in
  let input = String_util.pkcs7_padding input ~blocksize in
  let blocks = String_util.to_blocks input ~blocksize in
  List.folding_map blocks ~init:iv ~f:(fun prev_ciphertext next_plaintext ->
      let next_ciphertext =
        String_util.xor_raw prev_ciphertext next_plaintext
        |> Cstruct.of_string
        |> cipher
        |> Cstruct.to_string
      in
      next_ciphertext, next_ciphertext)
  |> String.concat

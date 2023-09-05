open! Core

let decrypt input ~blocksize ~key ~cipher =
  let cipher = cipher ~key in
  let blocks = String_util.to_blocks input ~blocksize in
  List.map blocks ~f:(fun ciphertext ->
      let plaintext =
        ciphertext
        |> Cstruct.of_string
        |> cipher
        |> Cstruct.to_string
      in
      plaintext)
  |> String.concat
  |> String_util.remove_pkcs7_padding

let encrypt input ~blocksize ~key ~cipher =
  let cipher = cipher ~key in
  let input = String_util.pkcs7_padding input ~blocksize in
  let blocks = String_util.to_blocks input ~blocksize in
  List.map blocks ~f:(fun plaintext ->
      Cstruct.of_string plaintext
      |> cipher
      |> Cstruct.to_string)
  |> String.concat

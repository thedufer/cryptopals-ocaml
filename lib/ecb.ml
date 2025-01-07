open! Core

let decrypt input ~blocksize ~key ~cipher =
  let cipher = cipher ~key in
  let blocks = String_util.to_blocks input ~blocksize in
  List.map blocks ~f:cipher
  |> String.concat
  |> String_util.remove_pkcs7_padding

let encrypt input ~blocksize ~key ~cipher =
  let cipher = cipher ~key in
  let input = String_util.pkcs7_padding input ~blocksize in
  let blocks = String_util.to_blocks input ~blocksize in
  List.map blocks ~f:cipher
  |> String.concat

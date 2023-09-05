open! Core

let single_char_cipher plaintext key =
  let len = String.length plaintext in
  let key = String.init len ~f:(Fn.const key) in
  String_util.xor_raw plaintext key

let multi_char_cipher plaintext ~key =
  let plaintext_len = String.length plaintext in
  let key =
    List.init (plaintext_len / String.length key + 1) ~f:(const key)
    |> String.concat
    |> String.sub ~pos:0 ~len:plaintext_len
  in
  String_util.xor_raw plaintext key

let find_single_char_cipher_key input =
  List.init 256 ~f:(fun i ->
      let char = Char.of_int_exn i in
      let result = single_char_cipher input char in
      let score = String_util.english_score result in
      (score, char))
  |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:fst)
  |> Option.value_exn
  |> snd

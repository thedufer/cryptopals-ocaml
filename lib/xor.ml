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

let find_multi_char_cipher_key input ~keysize =
  let num_whole_blocks = String.length input / keysize in
  let size_last_block = String.length input % keysize in
  List.init keysize ~f:(fun i ->
      let str =
        List.init (num_whole_blocks + (if i < size_last_block then 1 else 0)) ~f:(fun j ->
            String.get input (j * keysize + i))
        |> String.of_list
      in
      find_single_char_cipher_key str)
  |> String.of_list

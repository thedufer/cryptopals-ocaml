open! Core
open! Import

let oracle =
  let open Mirage_crypto.AES.ECB in
  let plaintext_suffix =
    String.filter ~f:(fun c -> not (Char.is_whitespace c))
      "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
       aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
       dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
       YnkK"
    |> String_util.base64_to_raw
  in
  let key = String_util.random_bytes 16 |> of_secret in
  fun plaintext_prefix ->
    let plaintext = plaintext_prefix ^ plaintext_suffix in
    let ciphertext = Ecb.encrypt plaintext ~blocksize:16 ~key ~cipher:encrypt in
    ciphertext

let find_plaintext_suffix (oracle : string -> string) =
  let blocksize, suffix_length = Cpa.get_blocksize_and_base_length oracle in
  let mode =
    let data =
      let orig_length_in_blocks = (oracle "" |> String.length) / blocksize in
      let base = String.init blocksize ~f:Char.of_int_exn in
      List.init (Int.max 10 (5 * orig_length_in_blocks)) ~f:(const base) |> String.concat
    in
    let ciphertext = oracle data in
    let blocks = String_util.to_blocks ciphertext ~blocksize in
    let unique_blocks = String.Set.of_list blocks in
    let ratio = (Set.length unique_blocks |> Int.to_float) /. (List.length blocks |> Int.to_float) in
    (if Float.O.(ratio < 0.5) then `ECB else `CBC)
  in
  print_s [%message (blocksize : int) (mode : [`ECB | `CBC]) (suffix_length : int)];
  (* everything else is based on this being ECB *)
  assert ([%equal: [`ECB | `CBC]] mode `ECB);
  let get_ciphertext_nth_block prefix n =
    oracle prefix |> String.sub ~pos:(n * blocksize) ~len:blocksize
  in
  let find_next_char known_prefix =
    let which_block = String.length known_prefix / blocksize in
    let prefix = String.init ((-1 * (String.length known_prefix + 1)) % blocksize) ~f:(const 'A') in
    let real_nth_block = get_ciphertext_nth_block prefix which_block in
    List.init 256 ~f:Char.of_int_exn
    |> List.find ~f:(fun last_char ->
        let this_nth_block = get_ciphertext_nth_block (prefix ^ known_prefix ^ String.of_char last_char) which_block in
        String.equal real_nth_block this_nth_block)
    |> Option.value_exn
  in
  List.init suffix_length ~f:Fn.id
  |> List.fold ~init:"" ~f:(fun acc _ ->
      let next_char = find_next_char acc in
      acc ^ String.of_char next_char)

let%expect_test "final" =
  find_plaintext_suffix oracle |> print_endline;
  [%expect {|
    ((blocksize 16) (mode ECB) (suffix_length 138))
    Rollin' in my 5.0
    With my rag-top down so my hair can blow
    The girlies on standby waving just to say hi
    Did you stop? No, I just drove by |}]

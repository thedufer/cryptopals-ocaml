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
  fun ?prefix_length () ->
    let key = String_util.random_bytes 16 |> of_secret in
    let prefix_length =
      match prefix_length with
      | None -> Random.int 32
      | Some l -> l
    in
    let plaintext_prefix = String_util.random_bytes prefix_length in
    fun plaintext_middle ->
      let plaintext = plaintext_prefix ^ plaintext_middle ^ plaintext_suffix in
      let ciphertext = Ecb.encrypt plaintext ~blocksize:16 ~key ~cipher:encrypt in
      ciphertext

let find_plaintext_suffix (oracle : string -> string) =
  let blocksize, base_length = Cpa.get_blocksize_and_base_length oracle in
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
  print_s [%message (blocksize : int) (mode : [`ECB | `CBC]) (base_length : int)];
  (* everything else is based on this being ECB *)
  assert ([%equal: [`ECB | `CBC]] mode `ECB);
  let prefix_length =
    let base_result = oracle "" |> String_util.to_blocks ~blocksize in
    let my_result =
      oracle ("B" ^ String.init (2 * blocksize - 2) ~f:(const 'A') ^ "B")
      |> String_util.to_blocks ~blocksize
    in
    let remove_common_prefix x y =
      let rec aux i x y =
        match x, y with
        | (x :: xs), (y :: ys) when String.equal x y -> aux (i + 1) xs ys
        | _ -> i, x, y
      in
      aux 0 x y
    in
    let removed_start, base_result, my_result = remove_common_prefix base_result my_result in
    let _removed_end, base_result, my_result =
      remove_common_prefix (List.rev base_result) (List.rev my_result)
    in
    let base_result, my_result = List.rev base_result, List.rev my_result in
    if List.length base_result = 0 then (
      assert (List.length my_result = 2);
      removed_start * blocksize
    ) else (
      assert (List.length my_result = 3);
      assert (List.length base_result = 1);
      (* it must be between removed_start * blocksize + 1 and removed_start * blocksize + 15 *)
      let all_A_ciphertext_block = List.nth_exn my_result 1 in
      let all_A_ciphertext_block_position = removed_start + 1 in
      let subblock_position =
        let min_addl_length =
          List.init 15 ~f:(( + ) 2)
          |> List.find_exn ~f:(fun i ->
              let next_block =
                oracle ("B" ^ String.init (2 * blocksize - 2 + i) ~f:(const 'A') ^ "B")
                |> String_util.to_blocks ~blocksize
                |> Fn.flip List.nth_exn (all_A_ciphertext_block_position + 1)
              in
              String.equal next_block all_A_ciphertext_block)
        in
        blocksize - min_addl_length + 1
      in
      removed_start * blocksize + subblock_position)
  in
  let suffix_length = base_length - prefix_length in
  print_s [%message (prefix_length : int) (suffix_length : int)];
  let addl_prefix = blocksize - (prefix_length % blocksize) in
  let prefix_blocks_to_ignore = (prefix_length + addl_prefix) / blocksize in
  (* TODO we've got the prefix length, now incorporate that into the suffix decrypt *)
  let get_ciphertext_nth_block prefix n =
    oracle ((String.init addl_prefix ~f:(const 'C')) ^ prefix)
    |> String.sub ~pos:((n + prefix_blocks_to_ignore) * blocksize) ~len:blocksize
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
  let first_result = find_plaintext_suffix (oracle ()) in
  print_endline first_result;
  List.init 32 ~f:Fn.id
  |> List.iter ~f:(fun prefix_length ->
      let this_result = find_plaintext_suffix (oracle ~prefix_length ()) in
      Expect_test_helpers_core.require_equal [%here] (module String) first_result this_result);
  [%expect {|
    ((blocksize 16) (mode ECB) (base_length 144))
    ((prefix_length 6) (suffix_length 138))
    Rollin' in my 5.0
    With my rag-top down so my hair can blow
    The girlies on standby waving just to say hi
    Did you stop? No, I just drove by

    ((blocksize 16) (mode ECB) (base_length 138))
    ((prefix_length 0) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 139))
    ((prefix_length 1) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 140))
    ((prefix_length 2) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 141))
    ((prefix_length 3) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 142))
    ((prefix_length 4) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 143))
    ((prefix_length 5) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 144))
    ((prefix_length 6) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 145))
    ((prefix_length 7) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 146))
    ((prefix_length 8) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 147))
    ((prefix_length 9) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 148))
    ((prefix_length 10) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 149))
    ((prefix_length 11) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 150))
    ((prefix_length 12) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 151))
    ((prefix_length 13) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 152))
    ((prefix_length 14) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 153))
    ((prefix_length 15) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 154))
    ((prefix_length 16) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 155))
    ((prefix_length 17) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 156))
    ((prefix_length 18) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 157))
    ((prefix_length 19) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 158))
    ((prefix_length 20) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 159))
    ((prefix_length 21) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 160))
    ((prefix_length 22) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 161))
    ((prefix_length 23) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 162))
    ((prefix_length 24) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 163))
    ((prefix_length 25) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 164))
    ((prefix_length 26) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 165))
    ((prefix_length 27) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 166))
    ((prefix_length 28) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 167))
    ((prefix_length 29) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 168))
    ((prefix_length 30) (suffix_length 138))
    ((blocksize 16) (mode ECB) (base_length 169))
    ((prefix_length 31) (suffix_length 138)) |}]

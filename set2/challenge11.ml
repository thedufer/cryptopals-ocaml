open! Core
open! Import

let random_string length =
  let bytes = Bytes.create length in
  List.init length ~f:(fun _ -> Random.char ())
  |> List.iteri ~f:(fun i c -> Bytes.set bytes i c);
  Bytes.to_string bytes

let oracle plaintext =
  let open Mirage_crypto.Cipher_block.AES.ECB in
  let mode = if Random.bool () then `ECB else `CBC in
  let key = random_string 16 |> Cstruct.of_string |> of_secret in
  let plaintext = random_string (Random.int 6 + 5) ^ plaintext ^ random_string (Random.int 6 + 5) in
  let ciphertext =
    match mode with
    | `ECB ->
      Ecb.encrypt plaintext ~blocksize:16 ~key ~cipher:encrypt
    | `CBC ->
      let iv = random_string 16 in
      Cbc.encrypt plaintext ~iv ~blocksize:16 ~key ~cipher:encrypt
  in
  (ciphertext, mode)

let guess_mode (oracle : string -> string * 'a) =
  let data =
    let base = String.init 16 ~f:Char.of_int_exn in
    List.init 10 ~f:(const base) |> String.concat
  in
  let ciphertext, real_result = oracle data in
  let blocks = String_util.to_blocks ciphertext ~blocksize:16 in
  let unique_blocks = String.Set.of_list blocks in
  let ratio = (Set.length unique_blocks |> Int.to_float) /. (List.length blocks |> Int.to_float) in
  (if Float.O.(ratio < 0.5) then `ECB else `CBC), real_result

let%expect_test "final" =
  List.init 100 ~f:(fun _ ->
      let guessed_mode, real_mode = guess_mode oracle in
      Expect_test_helpers_core.require [%here] ([%equal: [`CBC | `ECB]] real_mode guessed_mode))
  |> List.iter ~f:Fn.id;
  [%expect {| |}]
